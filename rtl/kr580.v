module kr580(

    // Шина данных
    input                clock,
    input                reset_n,
    input                locked,
    input        [ 7:0]  in,
    output       [15:0]  address,   // Указатель на адрес
    output  reg          we,        // Разрешить запись (высокий уровень)
    output  reg          pw,        // Port Write
    output  reg  [ 7:0]  out,

    // Interrupt
    input   wire         intr
);

localparam // Набор АЛУ
    ALU_ADD = 0, ALU_ADC = 1, ALU_SUB = 2, ALU_SBC = 3,
    ALU_AND = 4, ALU_XOR = 5, ALU_OR  = 6, ALU_CP  = 7;

localparam // Флаги и регистры
    REG_B  = 0, REG_C  = 1, REG_D = 2, REG_E = 3,
    REG_H  = 4, REG_L  = 5,            REG_A = 7,
    REG_HL = 2, REG_SP = 3, CF    = 0, NF    = 1,
    PF     = 2, AF     = 4, ZF    = 6, SF    = 7;

initial begin we  = 0; out = 0; end

// Указатель на необходимые данные
assign address = alt ? cp : pc;

// Управляющие регистры
reg  [ 3:0] t       = 0;        // Это t-state
reg  [ 2:0] m       = 0;        // Это m-state для префиксов
reg         ei      = 0;        // Enabled Interrupt
reg         ei_     = 0;        // Это необходимо для EI+RET конструкции
reg  [15:0] cp      = 0;
reg         alt     = 1'b0;     // =0 pc  =1 cp

// Регистры общего назначения
reg  [15:0] bc  = 16'hAFB1, de = 16'h0305, hl = 16'h0003;
reg  [15:0] pc  = 16'h0000, sp = 16'h0000;
reg  [ 1:0] im  = 2'b00;
reg  [ 7:0] i   = 8'h00,
            a   = 8'h0A,
            f   = 8'b00000000;
                 //  SZ A P C

// Сохраненный опкод
wire [ 7:0] opcode          = t ? latch : in;
reg  [ 7:0] latch           = 8'h00;
reg         prev_intr       = 1'b0;
reg         irq             = 1'b0;     // Исполнение запроса IRQ
reg  [ 2:0] irq_t           = 1'b0;     // Шаг исполнения IRQ

// Управление записью в регистры
reg         _b = 1'b0;      // Сигнал на запись 8 битного регистра
reg         _w = 1'b0;      // Сигнал на запись 16 битного регистра (_u:_l)
reg  [ 2:0] _n = 3'h0;      // Номер регистра
reg  [ 7:0] _l = 8'h00;     // Что писать
reg  [ 7:0] _u = 8'h00;     // Что писать
reg  [ 7:0] _f = 8'h00;     // Сохранение флага
reg         fw;             // Писать флаги
reg         ex_de_hl;

// Определение условий
wire        reg_hl  = (_n == 3'b110);
wire [15:0] signext = pc + 1 + {{8{in[7]}}, in[7:0]};
wire [3:0]  cc      = {f[CF], ~f[CF], f[ZF], ~f[ZF]};
wire        ccc     = (opcode[5:4] == 2'b00) & (f[ZF] == opcode[3]) | // NZ, Z,
                      (opcode[5:4] == 2'b01) & (f[CF] == opcode[3]) | // NC, C,
                      (opcode[5:4] == 2'b10) & (f[PF] == opcode[3]) | // PO, PE
                      (opcode[5:4] == 2'b11) & (f[SF] == opcode[3]) | // P, M
                       opcode == 8'b11_001_001 | // RET
                       opcode == 8'b11_000_011 | // JP
                       opcode == 8'b11_001_101;  // CALL

// -----------------------------------------------------------------------------
// Работа с регистрами
// -----------------------------------------------------------------------------

// Алиасы к проводам
wire [2:0] op20 = opcode[2:0];
wire [2:0] op53 = opcode[5:3];
wire [1:0] op54 = opcode[5:4];

// Вариант 1. Источник opcode[2:0]
wire [7:0] r20 =
    op20 == 3'h0 ? bc[15:8] : // B
    op20 == 3'h1 ? bc[ 7:0] : // C
    op20 == 3'h2 ? de[15:8] : // D
    op20 == 3'h3 ? de[ 7:0] : // E
    op20 == 3'h4 ? hl[15:8] : // H
    op20 == 3'h5 ? hl[ 7:0] : // L
    op20 == 3'h6 ? in : a;    // F, A

// Вариант 2. Источник opcode[5:3]
wire [7:0] r53 =
    op53 == 3'h0 ? bc[15:8] : // B
    op53 == 3'h1 ? bc[ 7:0] : // C
    op53 == 3'h2 ? de[15:8] : // D
    op53 == 3'h3 ? de[ 7:0] : // E
    op53 == 3'h4 ? hl[15:8] : // H
    op53 == 3'h5 ? hl[ 7:0] : // L
    op53 == 3'h6 ? in : a;    // (HL), A

// 16-битный регистр, источник opcode[5:4]
wire [15:0] r16 =
    op54 == 3'h0 ? bc :
    op54 == 3'h1 ? de :
    op54 == 3'h2 ? hl : sp;

// 16-битные операции HL + R16
wire [16:0] hladd = hl + r16;

// Инструкции INC и DEC
wire [8:0] r53id  = opcode[0] ? r53 - 1 : r53 + 1;  // Расчет результата
wire       r53ido = r53 == (127 + opcode[0]);       // Флаг overflow

// -----------------------------------------------------------------------------
// Исполнение инструкции
// -----------------------------------------------------------------------------

always @(posedge clock)
// Сброс
if (reset_n == 1'b0) begin t <= 0; pc <= 0; alt <= 0; end
// Процессинг
else if (locked) begin

    // Подготовка управляющих сигналов
    alt      <= 1'b0;
    _b       <= 1'b0;
    _w       <= 1'b0;
    we       <= 1'b0;
    pw       <= 1'b0;
    fw       <= 1'b0;
    ex_de_hl <= 1'b0;

    // Выполнение запроса IRQ
    if (irq_t) begin

        // Деактивация прерываний. Если halt, то PC = PC + 1
        // if (irq_t == 1) begin irq_t <= 2; ei <= 0; ei_ <= 0; if (halt) pc <= pc + 1; end

    end
    // Запуск прерывания
    else if (t == 0 && ei && (prev_intr ^ intr)) begin

        irq_t       <= 1;
        prev_intr   <= intr;

    end
    // Исполнение опкодов
    else begin

        t <= t + 1;

        // Запись опкода на первом такте
        if (t == 0) begin

            t     <= 1;           // По умолчанию, к следующему такту
            irq   <= 0;           // Сброс IRQ вектора
            latch <= in;          // Запомнить опкод
            ei    <= ei_;         // Сброс EI-триггера
            pc    <= pc + 1;

        end

        // Выполнять инструкции можно только при отсутствии IRQ
        casex (opcode)

        // 1 NOP
        8'b00_000_000: t <= 0;

        // 1+ DJNZ *
        8'b00_010_000: case (t)

            0: begin

                _b <= 1;
                _n <= REG_B;
                _l <= bc[15:8] - 1;

                if (bc[15:8] == 1) pc <= pc + 2;

            end
            1: begin t <= 0; pc <= signext; end

        endcase

        // 2 JR *
        8'b00_011_000: if (t == 1) begin t <= 0; pc <= signext; end

        // 1+ JR cc, *
        8'b00_1xx_000: case (t)

            0: begin if (!cc[opcode[4:3]]) begin t <= 0; pc <= pc + 2; end end
            1: begin t <= 0; pc <= signext; end

        endcase

        // 3 LD r, i16
        8'b00_xx0_001: case (t)

            0: begin _n <= opcode[5:4]; end
            1: begin pc <= pc + 1; t <= 2; _l <= in; end
            2: begin pc <= pc + 1; t <= 0; _u <= in; _w <= 1'b1; end

        endcase

        // 1 ADD HL, r
        8'b00_xx1_001: begin

            t  <= 0;
            _w <= 1; // 16-бит
            _n <= 2; // HL

            {_u, _l} <= hladd;

            _f[NF]  <= 1'b0;
            _f[CF]  <= hladd[16];
            _f[ZF]  <= hladd[15:0] == 0;
            _f[SF]  <= hladd[15];

        end

        // 2 LD (BC|DE), A
        8'b00_0x0_010: case (t)

            0: begin alt <= 1; cp <= opcode[4] ? de : bc; out <= a; we <= 1; end
            1: begin alt <= 0; t <= 0; end

        endcase

        // 2 LD A, (BC|DE)
        8'b00_0x1_010: case (t)

            0: begin alt <= 1; cp <= opcode[4] ? de : bc;  end
            1: begin t   <= 0; _n <= REG_A; _b <= 1; _l <= in; end

        endcase

        // 5 LD (**), HL
        8'b00_100_010: case (t)

            1: begin t <= 2; cp[7:0]  <= in; pc <= pc + 1; end
            2: begin t <= 3; cp[15:8] <= in; we <= 1; alt <= 1; out <= hl[ 7:0]; end
            3: begin t <= 4; cp <= cp + 1;   we <= 1; alt <= 1; out <= hl[15:8]; end
            4: begin t <= 0; pc <= pc + 1; end

        endcase

        // 5 LD HL, (**)
        8'b00_101_010: case (t)

            1: begin t <= 2; pc <= pc + 1; cp[ 7:0] <= in;    end
            2: begin t <= 3; pc <= pc + 1; cp[15:8] <= in;    alt <= 1; end
            3: begin t <= 4; _n <= REG_L; _b <= 1; _l <= in; alt <= 1; cp <= cp + 1; end
            4: begin t <= 0; _n <= REG_H; _b <= 1; _l <= in; end

        endcase

        // 4 LD (**), A
        8'b00_110_010: case (t)

            1: begin t <= 2; cp[ 7:0] <= in; pc <= pc + 1; end
            2: begin t <= 3; cp[15:8] <= in; pc <= pc + 1; we <= 1; alt <= 1; out <= a[7:0]; end
            3: begin t <= 0; end

        endcase

        // 4 LD A, (**)
        8'b00_111_010: case (t)

            1: begin t <= 2; pc <= pc + 1; cp[ 7:0] <= in; end
            2: begin t <= 3; pc <= pc + 1; cp[15:8] <= in; alt <= 1; end
            3: begin t <= 0; _b <= 1; _n <= REG_A; _l <= in; end

        endcase

        // 1 INC r16
        8'b00_000_011: begin t <= 0; {_u, _l} <= bc + 1; _n <= 0; _w <= 1; end
        8'b00_010_011: begin t <= 0; {_u, _l} <= de + 1; _n <= 1; _w <= 1; end
        8'b00_100_011: begin t <= 0; {_u, _l} <= hl + 1; _n <= 2; _w <= 1; end
        8'b00_110_011: begin t <= 0; {_u, _l} <= sp + 1; _n <= 3; _w <= 1; end

        // 1 DEC r16
        8'b00_001_011: begin t <= 0; {_u, _l} <= bc - 1; _n <= 0; _w <= 1; end
        8'b00_011_011: begin t <= 0; {_u, _l} <= de - 1; _n <= 1; _w <= 1; end
        8'b00_101_011: begin t <= 0; {_u, _l} <= hl - 1; _n <= 2; _w <= 1; end
        8'b00_111_011: begin t <= 0; {_u, _l} <= sp - 1; _n <= 3; _w <= 1; end

        // 3 INC|DEC (HL)
        8'b00_110_10x: case (t)

            0: begin cp <= hl; alt <= 1; end
            1: begin

                out <= r53id[7:0];
                _f  <= {r53id[7], r53id[7:0] == 8'b0, 1'b0, r53id[4] ^ r53[4], 1'b0, r53ido, 1'b0, r53id[8]};
                fw  <= 1;
                we  <= 1;
                alt <= 1;

            end
            2: t <= 0;

        endcase

        // 1 INC|DEC r8
        8'b00_xxx_10x: begin

            t   <= 0;
            _n  <= op53;
            _b  <= 1;
            _l  <= r53id[7:0];
            _f  <= {r53id[7], r53id[7:0] == 8'b0, 1'b0, r53id[4] ^ r53[4], 1'b0, r53ido, 1'b0, r53id[8]};
            fw  <= 1;

        end

        // 2* LD r, i8
        8'b00_xxx_110: case (t)

            0: begin _n <= op53; cp <= hl; end
            1: begin t <= reg_hl ? 2 : 0; pc <= pc + 1; _b <= ~reg_hl; we <= reg_hl; _l <= in; out <= in; alt <= 1; end
            2: begin t <= 0; end

        endcase

        // 1 RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
        8'b00_xxx_111: begin

            t  <= 0;
            fw <= 1;
            _n <= REG_A;
            _b <= 1;
            _l <= alu_sr;
            _f <= alu_sf;

        end

        // 1 HALT: Остановить процессор
        8'b01_110_110: begin pc <= pc; t <= 0; end

        // --------------------------

        // 2 LD r, (HL)
        8'b01_xxx_110: case (t)

            0: begin alt <= 1; cp <= hl; end
            1: begin t <= 0; _b <= 1; _n <= op53; _l <= in; end

        endcase

        // 2 LD (HL), r
        8'b01_110_xxx: case (t)

            0: begin alt <= 1; cp <= hl; we <= 1; out <= r20; end
            1: begin t <= 0; end

        endcase

        // 1 LD r, r
        8'b01_xxx_xxx: begin

            t  <= 0;
            _b <= 1;
            _n <= op53;
            _l <= r20;

        end

        // 2 <ALU> A, (HL)
        8'b10_xxx_110: case (t)

            0: begin cp <= hl; alt <= 1; end
            1: begin

                t   <= 0;
                fw  <= 1;
                _b  <= (op53 != ALU_CP);  // Запись в регистр A, кроме CP инструкции
                _l  <= alu_r;
                _f  <= alu_f;
                _n  <= REG_A;

            end

        endcase

        // 1 <ALU> A, r
        8'b10_xxx_xxx: begin

            t   <= 0;
            fw  <= 1;
            _b  <= (op53 != ALU_CP);
            _l  <= alu_r;
            _f  <= alu_f;
            _n  <= REG_A;

        end

        // --------------------------

        // 1/3 RET c | RET
        8'b11_001_001,
        8'b11_xxx_000: case (t)

            0: begin t <= ccc; alt <= ccc; cp <= sp; end
            1: begin t <= 2; pc[ 7:0] <= in; alt   <= 1; cp <= cp + 1; end
            2: begin t <= 0; pc[15:8] <= in; _w <= 1; {_u, _l} <= cp + 1; _n <= REG_SP; end

        endcase

        // 4 POP r16
        8'b11_xx0_001: case (t)

            0: begin cp <= sp; alt <= 1; end
            1: begin t <= 2; cp <= cp + 1; _l <= in; alt <= 1;  end
            2: begin t <= 3; cp <= cp + 1; _u <= in;

                 if (opcode[5:4] == 2'b11) // POP AF
                      begin _n <= REG_A; _b <= 1; _l <= in; fw <= 1'b1; _f <= _l; end
                 else begin _n <= opcode[5:4];    _w <= 1; end

            end
            3: begin t <= 0; _n <= REG_SP; _w <= 1; {_u, _l} <= cp; end

        endcase

        // 1 JP (HL)
        8'b11_101_001: case (t)

            0: begin pc <= hl; t <= 0; end

        endcase

        // 1 LD SP, HL
        8'b11_111_001: case (t)

            0: begin t <= 0; _n <= REG_SP; _w <= 1; {_u, _l} <= hl; end

        endcase

        // 4 JP c, ** | JP **
        8'b11_000_011,
        8'b11_xxx_010: case (t)

            1: begin t <= 2; pc <= pc + 1'b1; _l <= in; end
            2: begin t <= 3; pc <= pc + 1'b1; _u <= in; end
            3: begin t <= 0; if (ccc) pc <= {_u, _l}; end

        endcase

        // 2 OUT (*), A
        8'b11_010_011: case (t)

            1: begin t <= 0; pc <= pc + 1; cp <= in; alt <= 1; out <= a; pw <= 1; end

        endcase

        // 3 IN  A, (*)
        8'b11_011_011: case (t)

            1: begin t <= 2; pc <= pc + 1; cp <= in; alt <= 1; end
            2: begin t <= 0; _l <= in; _b <= 1; _n <= REG_A; end

        endcase

        // 5 EX (SP), HL
        8'b11_100_011: case (t)

            0: begin alt <= 1; cp <= sp; end
            1: begin alt <= 1; t <= 2; _l <= in; out <= hl[7:0]; we <= 1; end
            2: begin alt <= 1; t <= 3; cp <= cp + 1; end
            3: begin alt <= 1; t <= 4; _u <= in; _w <= 1; _n <= REG_HL; out <= hl[15:8]; we <= 1; end
            4: begin t <= 0; end

        endcase

        // 1 EX DE, HL
        8'b11_101_011: case (t)

            0: begin t <= 0; ex_de_hl <= 1; end

        endcase

        // 1 DI, EI
        8'b11_11x_011: begin t <= 0; ei_ <= opcode[3]; end

        // 3/6 CALL c, **
        8'b11_001_101,
        8'b11_xxx_100: case (t)

            1: begin t <= 2; pc <= pc + 1; _l <= in; end
            2: begin         pc <= pc + 1; _u <= in; cp <= sp;
                     t <= ccc ? 3 : 0; end
            3: begin t <= 4; out <= pc[15:8]; we <= 1; alt <= 1; cp <= cp - 1; end
            4: begin t <= 5; out <= pc[ 7:0]; we <= 1; alt <= 1; cp <= cp - 1; end
            5: begin t <= 0; _w <= 1; _n <= REG_SP; pc <= {_u, _l}; {_u, _l} <= cp; end

        endcase

        // 3 PUSH r16
        8'b11_xx0_101: case (t)

            0: begin

                alt <= 1;
                we  <= 1;
                cp  <= sp - 1;
                out <= (op54 == 2'b11) ? a : r16[15:8];
                _w  <= 1;
                _n  <= REG_SP;
                {_u, _l} <= sp - 2;

            end

            1: begin

                t   <= 2;
                alt <= 1;
                we  <= 1;
                cp  <= cp - 1;
                out <= (op54 == 2'b11) ? f : r16[ 7:0];

            end

            2: t <= 0;

        endcase

        // 2 <ALU> A, i8
        8'b11_xxx_110: case (t)

            // Так как op20 равно 6, то в _r20 = in
            1: begin

                t   <= 0;
                pc  <= pc + 1;
                fw  <= 1;
                _b  <= (op53 != ALU_CP);
                _l  <= alu_r;
                _f  <= alu_f;
                _n  <= REG_A;

            end

        endcase

        // 4 RST #
        8'b11_xxx_111: case (t)

            0: begin cp <= sp; end
            1: begin t <= 2; out <= pc[15:8]; we <= 1; cp <= cp - 1; alt <= 1; end
            2: begin t <= 3; out <= pc[ 7:0]; we <= 1; cp <= cp - 1; alt <= 1; end
            3: begin t <= 0; _w <= 1; _n <= REG_SP; {_u, _l} <= cp; pc <= {op53, 3'b000}; end

        endcase

        endcase

    end

end

// -----------------------------------------------------------------------------
// Арифметико-логическое устройство
// -----------------------------------------------------------------------------

wire carry  =   alu_r[8];
wire sign   =   alu_r[7];
wire zero   = ~|alu_r[7:0];
wire prty   = ~^alu_r[7:0];
wire aux    =  a[4] ^ r20[4] ^ alu_r[4];
wire fadd   = (a[7] == r20[7]) && (a[7] != alu_r[7]);
wire fsub   = (a[7] != r20[7]) && (a[7] != alu_r[7]);
wire is_add = op53 == ALU_ADD || op53 == ALU_ADC;
wire is_lgc = op53 == ALU_AND || op53 == ALU_OR || op53 == ALU_XOR;

// Определение результата
wire [16:0] alu_r =
    op53 == ALU_ADD ? a + r20 :
    op53 == ALU_ADC ? a + r20 + f[CF] :
    op53 == ALU_SBC ? a - r20 - f[CF] :
    op53 == ALU_AND ? a & r20 :
    op53 == ALU_XOR ? a ^ r20 :
    op53 == ALU_OR  ? a | r20 : a - r20; // OR; SUB, CP

// Вычисление флагов
wire [7:0]  alu_f =
    is_lgc? {sign, zero, 3'b000, prty, 2'b00} : // AND, OR, XOR
            {sign, zero, 1'b0,   aux, 1'b0, (is_add ? fadd : fsub), 1'b0, carry};

// -----------------------------------------------------------------------------
// Сдвиги и работа над регистром A
// -----------------------------------------------------------------------------

// Отдельно, вычисление DAA
wire [7:0] alu_sr_daa =
    (f[NF]) ? a - ((f[AF] | (a[3:0] > 4'h9)) ? 8'h06 : 0) - ((f[CF] | (a[7:0] > 8'h99)) ? 8'h60 : 0) :
              a + ((f[AF] | (a[3:0] > 4'h9)) ? 8'h06 : 0) + ((f[CF] | (a[7:0] > 8'h99)) ? 8'h60 : 0);

// Промежуточное вычисление флагов результата
wire        alu_sr_prty = ~^alu_sr[7:0];
wire        alu_sr_zero = alu_sr[7:0] == 8'b0;
wire [7:0]  alu_sf_daa  = {alu_sr[7], alu_sr_zero, alu_sr[5], a[4] ^ alu_sr[4], alu_sr[3], alu_sr_prty, f[NF], f[CF] | (a > 8'h99)};

// Вычисление сдвигов
wire [7:0] alu_sr =
    op53 == 3'h0 ? {a[6:0], a[  7]} : // RLCA
    op53 == 3'h1 ? {a[0],   a[7:1]} : // RRCA
    op53 == 3'h2 ? {a[6:0], f[ CF]} : // RLA
    op53 == 3'h3 ? {f[CF],  a[7:1]} : // RRA
    op53 == 3'h4 ? alu_sr_daa :       // DAA
    op53 == 3'h5 ? ~a : a;            // CPL : (SCF, CCF)

// Вычисление флагов
wire [7:0] alu_sf =
    op53 == 3'h0 || op53 == 3'h2 ? {alu_sr[7], alu_sr_zero, 3'b000, alu_sr_prty, 1'b1, a[7]} : // RLCA, RLA
    op53 == 3'h1 || op53 == 3'h3 ? {alu_sr[7], alu_sr_zero, 3'b000, alu_sr_prty, 1'b1, a[0]} : // RRCA, RRA
    op53 == 3'h4 ? alu_sf_daa : // DAA
    op53 == 3'h5 ? {f[SF], f[ZF], 3'b010, f[PF], 1'b1, f[CF]} :
    op53 == 3'h6 ? {f[SF], f[ZF], 1'b0, f[AF], 1'b0, f[PF], 2'b11} :
                   {f[SF], f[ZF], 1'b0, f[AF], 1'b0, f[PF], 1'b1, f[CF] ^ 1'b1};

// -----------------------------------------------------------------------------
// Запись в регистры
// -----------------------------------------------------------------------------

always @(negedge clock)
begin

    if (ex_de_hl) begin de <= hl; hl <= de; end
    // Запись в 16-битные регистры
    else if (_w) case (_n)

        3'h0: bc <= {_u, _l};
        3'h1: de <= {_u, _l};
        3'h2: hl <= {_u, _l};
        3'h3: sp <= {_u, _l};

    endcase
    // Запись в 8 битные регистры
    else if (_b) case (_n)

        3'h0: bc[15:8] <= _l;
        3'h1: bc[ 7:0] <= _l;
        3'h2: de[15:8] <= _l;
        3'h3: de[ 7:0] <= _l;
        3'h4: hl[15:8] <= _l;
        3'h5: hl[ 7:0] <= _l;
        3'h7: a <= _l;

    endcase

    // Сохранение флагов
    if (fw) f <= _f;

end

endmodule
