/* verilator lint_off WIDTHTRUNC */
/* verilator lint_off WIDTHEXPAND */
/* verilator lint_off CASEX */
/* verilator lint_off CASEOVERLAP */
/* verilator lint_off CASEINCOMPLETE */

module kr580
(
    // Шина данных
    input               clock,
    input               reset_n,
    input               locked,
    input        [ 7:0] in,
    output       [15:0] address,    // Указатель на адрес
    output  reg         we,         // Разрешить запись (высокий уровень)
    output  reg         pw,         // Port write
    output  reg         pr,         // Port read (на IN значение порта address)
    output  reg  [ 7:0] out,

    // Регистры
    output  reg  [15:0] pc,
    output  reg  [15:0] sp,
    output  reg  [15:0] bc,
    output  reg  [15:0] de,
    output  reg  [15:0] hl,
    output       [15:0] af,

    // Interrupt
    input   wire        intr
);

assign af = {a, f};

// Набор АЛУ
localparam
    ALU_ADD = 0, ALU_ADC = 1, ALU_SUB = 2, ALU_SBC = 3,
    ALU_AND = 4, ALU_XOR = 5, ALU_OR  = 6, ALU_CP  = 7,
    ALU_RLC = 0, ALU_RRC = 1, ALU_RL  = 2, ALU_RR  = 3,
    ALU_DAA = 4, ALU_CPL = 5, ALU_SCF = 6, ALU_CCF = 7;

// Регистры
localparam
    REG_B  = 0, REG_C  = 1, REG_D  = 2, REG_E = 3,
    REG_H  = 4, REG_L  = 5, REG_M  = 6, REG_A = 7,
    REG_BC = 0, REG_DE = 1, REG_HL = 2, REG_SP = 3;

// Флаги
localparam CF = 0, NF = 1, PF = 2, AF = 4, ZF = 6, SF = 7;

// Специальные
localparam
    EX_DE_HL = 1, EX_AF  = 2,
    INCSP2   = 3, DECSP2 = 4,
    EXX      = 5;

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

// Регистры. Есть также регистры AF'BC'DE'HL' из дополнительного набора
reg  [15:0] bc_ = 16'h0000, de_ = 16'h0000, hl_ = 16'h0000; // Дополнительный
reg  [ 1:0] im  = 2'b00;
reg  [ 7:0] i   = 8'h00,
            a   = 8'h0A,       a_ = 8'hFF,
            f   = 8'b11000100, f_ = 8'h00;
                 //  SZ5A3PNC

// Сохраненный опкод
wire [ 7:0] opcode          = t ? latch : in;
reg  [ 7:0] latch           = 8'h00;
reg         intrp           = 1'b0;

// Управление записью в регистры
reg         _b = 1'b0;      // Сигнал на запись 8 битного регистра
reg         _w = 1'b0;      // Сигнал на запись 16 битного регистра (_u:_l)
reg  [ 2:0] _n = 3'h0;      // Номер регистра
reg  [ 7:0] _l = 8'h00;     // Что писать
reg  [ 7:0] _u = 8'h00;     // Что писать
reg  [ 7:0] _f = 8'h00;     // Сохранение флага
reg         fw;             // Писать флаги
reg  [ 2:0] spec;           // Специальные операции; EX DE,HL; EX AF,AF'

// Определение условий
wire [15:0] pci = pc + 1;
wire [15:0] pcn = pci + {{8{in[7]}}, in[7:0]};
wire [7:0]  ccc = {f[SF], ~f[SF], f[PF], ~f[PF], f[CF], ~f[CF], f[ZF], ~f[ZF]};
wire        ccx = ccc[op53] || opcode == 8'hC9 || opcode == 8'hC3 || opcode == 8'hCD; // CCC; RET, JP, CALL

// -----------------------------------------------------------------------------
// Работа с регистрами
// -----------------------------------------------------------------------------

// Алиасы к проводам
wire [2:0] op20 = opcode[2:0];
wire [2:0] op53 = opcode[5:3];
wire [1:0] op54 = opcode[5:4];

// Вариант 1. Источник opcode[2:0]
wire [7:0] r20 =
    op20 == REG_B ? bc[15:8] : op20 == REG_C ? bc[ 7:0] :
    op20 == REG_D ? de[15:8] : op20 == REG_E ? de[ 7:0] :
    op20 == REG_H ? hl[15:8] : op20 == REG_L ? hl[ 7:0] :
    op20 == REG_M ? in : a;

// Вариант 2. Источник opcode[5:3]
wire [7:0] r53 =
    op53 == REG_B ? bc[15:8] : op53 == REG_C ? bc[ 7:0] :
    op53 == REG_D ? de[15:8] : op53 == REG_E ? de[ 7:0] :
    op53 == REG_H ? hl[15:8] : op53 == REG_L ? hl[ 7:0] :
    op53 == REG_M ? in : a;

// 16-битный регистр, источник opcode[5:4]
wire [15:0] r16 =
    op54 == REG_BC ? bc :
    op54 == REG_DE ? de :
    op54 == REG_HL ? hl : sp;

// 16-битные операции HL + R16
wire [16:0] hladd = hl + r16;

// Инструкции INC и DEC
wire [8:0] incdec = opcode[0] ? r53 - 1 : r53 + 1;  // Расчет результата
wire       r53ido = r53 == (127 + opcode[0]);       // Флаг overflow
wire [7:0] idflag = {incdec[7], incdec[7:0] == 8'b0, 1'b0, incdec[4] ^ r53[4], 1'b0, r53ido, 1'b0, incdec[8]};

// -----------------------------------------------------------------------------
// Исполнение инструкции
// -----------------------------------------------------------------------------

always @(posedge clock)
// Сброс
if (reset_n == 1'b0) begin t <= 0; pc <= 0; alt <= 0; end
// Процессинг
else if (locked) begin

    // Подготовка управляющих сигналов
    alt  <= 0;
    _b   <= 0;
    _w   <= 0;
    we   <= 0;
    pw   <= 0;
    pr   <= 0;
    fw   <= 0;
    spec <= 0;

    // Запуск прерывания RST #38; IM 2 режима нет
    if (t == 0 && ei && (intrp != intr)) begin

        t        <= 1;
        alt      <= 1;
        we       <= 1;
        cp       <= sp - 1;
        intrp    <= intr;
        out      <= (in == 8'h76) ? pci[15:8] : pc[15:7]; // HALT?
        latch    <= 8'hFF;
        {ei,ei_} <= 0;

        // HALT добавляет PC+1
        if (in == 8'h76) pc <= pc + 1;

    end
    // Исполнение опкодов
    else begin

        t <= t + 1;

        // Запись опкода на первом такте
        if (t == 0) begin

            latch <= in;        // Запомнить опкод
            ei    <= ei_;       // Сброс EI-триггера
            pc    <= pc + 1;    // По умолчанию PC+1 на T=0

        end

        // ---------------------------------------------------------------------
        // Интерпретатор
        // ---------------------------------------------------------------------

        casex (opcode)

        // 1 NOP
        8'b00_000_000: t <= 0;

        // 1 EX AF,AF'
        8'b00_001_000: begin t <= 0; spec <= EX_AF; end

        // 1+ DJNZ *
        8'b00_010_000: case (t)

            0: begin

                _b <= 1;
                _n <= REG_B;
                _l <= bc[15:8] - 1;

                if (bc[15:8] == 1) begin pc <= pc + 2; t <= 0; end

            end
            1: begin t <= 0; pc <= pcn; end

        endcase

        // 2  JR *
        // 1+ JR cc, *
        8'b00_011_000,
        8'b00_1xx_000: case (t)

            0: if (!ccc[opcode[4:3]] && opcode[5]) begin t <= 0; pc <= pc + 2; end
            1: begin t <= 0; pc <= pcn; end

        endcase

        // 3 LD r, i16
        8'b00_xx0_001: case (t)

            0: begin _n <= opcode[5:4]; end
            1: begin pc <= pc + 1; _l <= in; end
            2: begin pc <= pc + 1; _u <= in; _w <= 1; t <= 0; end

        endcase

        // 1 ADD HL, r
        8'b00_xx1_001: begin

            t  <= 0;
            _w <= 1;
            _n <= REG_HL;

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

            1: begin cp[ 7:0] <= in; pc <= pc + 1; end
            2: begin cp[15:8] <= in; we <= 1; alt <= 1; out <= hl[ 7:0]; pc <= pc + 1;  end
            3: begin cp <= cp + 1;   we <= 1; alt <= 1; out <= hl[15:8]; end
            4: begin t <= 0; end

        endcase

        // 5 LD HL, (**)
        8'b00_101_010: case (t)

            1: begin pc <= pc + 1; cp[ 7:0] <= in;   end
            2: begin pc <= pc + 1; cp[15:8] <= in;   alt <= 1; end
            3: begin _n <= REG_L; _b <= 1; _l <= in; alt <= 1; cp <= cp + 1; end
            4: begin _n <= REG_H; _b <= 1; _l <= in; t <= 0; end

        endcase

        // 4 LD (**), A
        8'b00_110_010: case (t)

            1: begin cp[ 7:0] <= in; pc <= pc + 1; out <= a; end
            2: begin cp[15:8] <= in; pc <= pc + 1; we <= 1; alt <= 1; end
            3: begin t <= 0; end

        endcase

        // 4 LD A, (**)
        8'b00_111_010: case (t)

            1: begin pc <= pc + 1; cp[ 7:0] <= in; end
            2: begin pc <= pc + 1; cp[15:8] <= in; alt <= 1; end
            3: begin _b <= 1; _n <= REG_A; _l <= in; t <= 0; end

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

                _f  <= idflag;
                out <= incdec;
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
            _l  <= incdec;
            _f  <= idflag;
            fw  <= 1;

        end

        // 2+ LD r, i8
        8'b00_xxx_110: case (t)

            0: cp <= hl;
            1: begin

                t   <= op53 == REG_M ? 2 : 0;
                we  <= op53 == REG_M;
                alt <= op53 == REG_M;
                _b  <= op53 != REG_M;
                _n  <= op53;
                _l  <= in;
                out <= in;
                pc  <= pc + 1;

            end

            2: t <= 0;

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
                _b  <= (op53 != ALU_CP);
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

            0: begin t <= ccx; alt <= ccx; cp <= sp; end
            1: begin pc[ 7:0] <= in; alt <= 1; cp <= cp + 1; end
            2: begin pc[15:8] <= in; spec <= INCSP2; t <= 0; end

        endcase

        // 3 POP r16
        8'b11_xx0_001: case (t)

            0: begin cp <= sp; alt <= 1; spec <= INCSP2; end
            1: begin _l <= in; alt <= 1; cp <= cp + 1; end
            2: begin

                _u <= in;
                t  <= 0;

                if (opcode[5:4] == 2'b11)
                     begin _n <= REG_A; _b <= 1; _l <= in; fw <= 1'b1; _f <= _l; end // POP AF
                else begin _n <= opcode[5:4];    _w <= 1; end

            end

        endcase

        // 1 EXX
        // 1 JP (HL)
        // 1 LD SP, HL
        8'b11_011_001: begin t <= 0; spec <= EXX; end
        8'b11_101_001: begin t <= 0; pc <= hl; end
        8'b11_111_001: begin t <= 0; _w <= 1; _n <= REG_SP; {_u, _l} <= hl; end

        // 1/3 JP c, ** | JP **
        8'b11_000_011,
        8'b11_xxx_010: case (t)

            0: if (!ccx) begin t <= 0; pc <= pc + 3; end
            1: begin _l <= in; pc <= pc + 1'b1; end
            2: begin pc <= {in, _l}; t <= 0; end

        endcase

        // 3 OUT (*), A
        8'b11_010_011: case (t)

            1: begin

                pw  <= 1;
                alt <= 1;
                cp  <= in;
                pc  <= pc + 1;
                out <= a;

            end
            2: t <= 0;

        endcase

        // 3 IN  A, (*)
        8'b11_011_011: case (t)

            1: begin pc <= pc + 1; cp <= in; pr <= 1; alt <= 1; end
            2: begin _b <= 1; _n <= REG_A; _l <= in; t <= 0; end

        endcase

        // 5 EX (SP), HL
        8'b11_100_011: case (t)

            0: begin alt <= 1; cp <= sp; end
            1: begin alt <= 1; _l <= in; out <= hl[ 7:0]; we <= 1; end
            2: begin alt <= 1; cp <= cp + 1; end
            3: begin alt <= 1; _u <= in; out <= hl[15:8]; we <= 1; _w <= 1; _n <= REG_HL; end
            4: begin t <= 0; end

        endcase

        // 1 EX DE, HL
        // 1 DI, EI
        8'b11_101_011: begin t <= 0; spec <= EX_DE_HL; end
        8'b11_11x_011: begin t <= 0; ei_ <= opcode[3]; end

        // 1/5 CALL c, **
        8'b11_001_101,
        8'b11_xxx_100: case (t)

            0: if (!ccx) begin t <= 0; pc <= pc + 3; end
            1: begin _l <= in; pc <= pc + 1; end
            2: begin

                alt  <= 1;
                we   <= 1;
                cp   <= sp - 1;
                out  <= pci[15:8];
                _u   <= in;

            end
            3: begin

                alt  <= 1;
                we   <= 1;
                out  <= pci[ 7:0];
                cp   <= cp - 1;
                pc   <= {_u, _l};
                spec <= DECSP2;

            end
            4: t <= 0;

        endcase

        // 3 PUSH r16
        8'b11_xx0_101: case (t)

            0: begin

                alt  <= 1;
                we   <= 1;
                cp   <= sp - 1;
                out  <= (op54 == 2'b11) ? a : r16[15:8];
                spec <= DECSP2;

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

        // 3 RST #
        8'b11_xxx_111: case (t)

            0: begin out <= pci[15:8]; cp <= sp - 1; we <= 1; alt <= 1; end
            1: begin out <= pc [ 7:0]; cp <= cp - 1; we <= 1; alt <= 1; end
            2: begin spec <= DECSP2; pc <= {op53, 3'b000}; t <= 0; end

        endcase

        endcase

    end

end

// -----------------------------------------------------------------------------
// Арифметико-логическое устройство
// -----------------------------------------------------------------------------

wire is_add = (op53 == ALU_ADD) || (op53 == ALU_ADC);
wire is_lgc = (op53 == ALU_AND) || (op53 == ALU_OR) || op53 == ALU_XOR;

// Определение результата
wire [16:0] alu_r =
    op53 == ALU_ADD ? a + r20 :
    op53 == ALU_ADC ? a + r20 + f[CF] :
    op53 == ALU_SBC ? a - r20 - f[CF] :
    op53 == ALU_AND ? a & r20 :
    op53 == ALU_XOR ? a ^ r20 :
    op53 == ALU_OR  ? a | r20 : a - r20; // OR; SUB, CP

// Вычисление флагов
wire carry  =   alu_r[8];
wire sign   =   alu_r[7];
wire zero   = ~|alu_r[7:0];
wire prty   = ~^alu_r[7:0];
wire aux    =  a[4] ^ r20[4] ^ alu_r[4];
wire over   = (a[7] ^ r20[7] ^ is_add) && (a[7] != alu_r[7]);

wire [7:0]  alu_f =
//           SF    ZF    F5        AF    F3        PF/OF    NF    CF
    is_lgc? {sign, zero, alu_r[5], 1'b0, alu_r[3], prty,    1'b0, 1'b0} : // AND, OR, XOR
            {sign, zero, alu_r[5], aux,  alu_r[3], over, !is_add, carry}; // ADD, ADC, SUB, SBC, CP

// -----------------------------------------------------------------------------
// Сдвиги и работа над регистром A
// -----------------------------------------------------------------------------

wire [7:0] daa = // Коррекция BCD-чисел после сложения и вычитания
    (f[NF]) ? a - ((f[AF] | (a[3:0] > 4'h9)) ? 8'h06 : 0) - ((f[CF] | (a[7:0] > 8'h99)) ? 8'h60 : 0) :
              a + ((f[AF] | (a[3:0] > 4'h9)) ? 8'h06 : 0) + ((f[CF] | (a[7:0] > 8'h99)) ? 8'h60 : 0);

// Промежуточное вычисление флагов результата
wire sr_prty = ~^alu_sr[7:0];
wire sr_zero =   alu_sr[7:0] == 8'b0;

// Вычисление сдвигов
wire [7:0] alu_sr =
    op53 == ALU_RLC ? {a[6:0], a[  7]} :
    op53 == ALU_RRC ? {a[0],   a[7:1]} :
    op53 == ALU_RL  ? {a[6:0], f[ CF]} :
    op53 == ALU_RR  ? {f[CF],  a[7:1]} :
    op53 == ALU_DAA ? daa :
    op53 == ALU_CPL ? ~a : a;

// Вычисление флагов
wire [7:0] alu_sf =
    op53 == ALU_RLC || op53 == ALU_RL ? {alu_sr[7], sr_zero, 3'b000, sr_prty, 1'b1, a[7]} :
    op53 == ALU_RRC || op53 == ALU_RR ? {alu_sr[7], sr_zero, 3'b000, sr_prty, 1'b1, a[0]} :
    op53 == ALU_DAA ? {daa[7], sr_zero, daa[5], a[4] ^ daa[4], daa[3], sr_prty, f[NF], f[CF] | (a > 8'h99)} :
    op53 == ALU_CPL ? {f[SF], f[ZF], 1'b0, 1'b1,  1'b0, f[PF], 1'b1,  f[CF]} :
    op53 == ALU_SCF ? {f[SF], f[ZF], 1'b0, f[AF], 1'b0, f[PF], 1'b1,   1'b1} :
                      {f[SF], f[ZF], 1'b0, f[AF], 1'b0, f[PF], 1'b1, !f[CF]};

// -----------------------------------------------------------------------------
// Запись в регистры
// -----------------------------------------------------------------------------

always @(negedge clock)
begin

    // Сохранение флагов
    if (fw) f <= _f;

    // Сохранение регистров
    case (spec)
    INCSP2:   sp <= sp + 2;
    DECSP2:   sp <= sp - 2;
    EXX:      begin {bc, de, hl, bc_, de_, hl_} <= {bc_, de_, hl_, bc, de, hl}; end // EXX
    EX_AF:    begin {a, f, a_, f_} <= {a_, f_, a, f}; end   // EX AF,AF'
    EX_DE_HL: begin {de, hl} <= {hl, de}; end               // EX DE,HL
    default:

        // Запись в 16-битные регистры
        if (_w) case (_n)

            REG_BC: bc <= {_u, _l};
            REG_DE: de <= {_u, _l};
            REG_HL: hl <= {_u, _l};
            REG_SP: sp <= {_u, _l};

        endcase
        // Запись в 8 битные регистры
        else if (_b) case (_n)

            REG_B: bc[15:8] <= _l; REG_C: bc[ 7:0] <= _l;
            REG_D: de[15:8] <= _l; REG_E: de[ 7:0] <= _l;
            REG_H: hl[15:8] <= _l; REG_L: hl[ 7:0] <= _l;
            REG_A: a        <= _l;

        endcase

    endcase

end

endmodule
