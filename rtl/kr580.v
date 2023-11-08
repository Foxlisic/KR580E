module kr580(

    // Шина данных
    input                clock,
    input                reset_n,
    input                locked,
    input        [ 7:0]  in,
    output       [15:0]  address,         // Указатель на адрес
    output  reg          we,       // Разрешить запись (высокий уровень)
    output  reg  [ 7:0]  out,

    // Порты
    output  reg  [ 7:0]  pin_pa,
    input   wire [ 7:0]  pin_pi,
    output  reg  [ 7:0]  pin_po,
    output  reg          pin_pw,

    // Interrupt
    input   wire         intr

);

localparam

    // Базовый набор
    ALU_ADD = 0,    ALU_ADC = 1,    ALU_SUB = 2,    ALU_SBC = 3,
    ALU_AND = 4,    ALU_XOR = 5,    ALU_OR  = 6,    ALU_CP  = 7,
    ALU_RLC = 8,    ALU_RRC = 9,    ALU_RL  = 10,   ALU_RR  = 11,
    ALU_DAA = 12,   ALU_CPL = 13,   ALU_SCF = 14,   ALU_CCF = 15;

`define CF 0
`define NF 1
`define PF 2
`define AF 4
`define ZF 6
`define SF 7

`define REG_B       0
`define REG_C       1
`define REG_D       2
`define REG_E       3
`define REG_H       4
`define REG_L       5
`define REG_F       6
`define REG_A       7

`define REG_HL      2
`define REG_SP      3

initial begin

    we = 0;
    out   = 0;
    pin_pa  = 0;
    pin_po  = 0;

end

// Указатель на необходимые данные
assign address = alt ? cp : pc;

// Управляющие регистры
reg  [ 3:0] t       = 0;        // Это t-state
reg  [ 2:0] m       = 0;        // Это m-state для префиксов
reg         halt    = 0;        // Процессор остановлен
reg         ei      = 0;        // Enabled Interrupt
reg         ei_     = 0;        // Это необходимо для EI+RET конструкции
reg  [15:0] cp      = 0;
reg         alt     = 1'b0;     // =0 pc  =1 cp

// Регистры общего назначения
reg  [15:0] bc = 16'h0000, de = 16'h0000, hl = 16'h0000;
reg  [15:0] pc = 16'h0000, sp = 16'h0000;
reg  [ 1:0] im = 2'b00;
reg  [ 7:0] i  = 8'h00,
            a  = 8'h00,
            f  = 8'b01000000;
                //  SZ A P C

// Сохраненный опкод
wire [ 7:0] opcode          = t ? latch : in;
reg  [ 7:0] latch           = 8'h00;
reg         prev_intr       = 1'b0;
reg         irq             = 1'b0;     // Исполнение запроса IRQ
reg  [ 2:0] irq_t           = 1'b0;     // Шаг исполнения

// Управление записью в регистры
reg         _b = 1'b0;       // Сигнал на запись 8 битного регистра
reg         _w = 1'b0;       // Сигнал на запись 16 битного регистра (_u:reg_v)
reg  [ 2:0] _n = 3'h0;       // Номер регистра
reg  [ 7:0] _l = 8'h00;      // Что писать
reg  [ 7:0] _u = 8'h00;      // Что писать
reg  [ 7:0] _f = 8'h00;      // Сохранение флага
reg  [ 7:0] _r8;             // _r8  = regs8 [ _n ]
reg  [15:0] _r16;            // _r16 = regs16[ _n ]
reg         fw;                 // Писать флаги
reg         ex_de_hl;

// Определение условий
wire        reg_hl  = (_n == 3'b110);
wire [15:0] signext = pc + 1 + {{8{in[7]}}, in[7:0]};
wire [3:0]  cc      = {f[`CF], ~f[`CF], f[`ZF], ~f[`ZF]};
wire        ccc     = (opcode[5:4] == 2'b00) & (f[`ZF] == opcode[3]) | // NZ, Z,
                      (opcode[5:4] == 2'b01) & (f[`CF] == opcode[3]) | // NC, C,
                      (opcode[5:4] == 2'b10) & (f[`PF] == opcode[3]) | // PO, PE
                      (opcode[5:4] == 2'b11) & (f[`SF] == opcode[3]) | // P, M
                       opcode == 8'b11_001_001 | // RET
                       opcode == 8'b11_000_011 | // JP
                       opcode == 8'b11_001_101;  // CALL

// Арифметическое-логическое устройство
reg  [ 8:0] alu_r;
reg  [ 4:0] alu_m;
reg  [ 7:0] alu_f;
reg  [ 7:0] op1, op2;

// Исполнение инструкции
always @(posedge clock)
if (locked) begin

    // Подготовка управляющих сигналов
    alt      <= 1'b0;
    _b       <= 1'b0;
    _w       <= 1'b0;
    we       <= 1'b0;
    pin_pw   <= 1'b0;
    halt     <= 1'b0;
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

        t  <= t  + 1;

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

        // 1/2 DJNZ *
        8'b00_010_000: case (t)

            0: begin

                _b <= 1;
                _n <= `REG_B;
                _l <= bc[15:8] - 1;

                if (bc[15:8] == 1) pc <= pc + 2;

            end
            1: begin t <= 0; pc <= signext; end

        endcase

        // 2 JR *
        8'b00_011_000: if (t == 1) begin t <= 0; pc <= signext; end

        // 1|2 JR cc, *
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

        // 4 ADD HL, r
        8'b00_xx1_001: case (t)

            0: begin _n <= {opcode[5:4], 1'b1}; end
            1: begin

                t       <= 2;
                alu_m   <= ALU_ADD;
                _n      <= {opcode[5:4], 1'b0};
                _f      <= f;
                op1     <= hl[7:0];
                op2     <= _r8;

            end
            2: begin

                t       <= 3;
                alu_m   <= ALU_ADC;
                op1     <= hl[15:8];
                op2     <= _r8;
                _n      <= `REG_L;
                _b      <= 1'b1;
                _l      <= alu_r[7:0];
                _f[0]   <= alu_f[`CF];
                fw      <= 1'b1;

            end
            3: begin

                t       <= 0;
                _n      <= `REG_H;
                _l      <= alu_r[7:0];
                _b      <= 1'b1;
                fw      <= 1'b1;
                _f[`AF] <= alu_f[`AF];
                _f[`CF] <= alu_f[`CF];
                _f[`SF] <= alu_f[`SF];

            end

        endcase

        // 2 LD (BC|DE), A
        8'b00_0x0_010: case (t)

            0: begin alt <= 1; cp <= opcode[4] ? de : bc; out <= a; we <= 1; end
            1: begin alt <= 0; t <= 0; end

        endcase

        // 2 LD A, (BC|DE)
        8'b00_0x1_010: case (t)

            0: begin alt <= 1; cp <= opcode[4] ? de : bc;  end
            1: begin t <= 0; _n <= `REG_A; _b <= 1; _l <= in; end

        endcase

        // 4 LD (**), HL
        8'b00_100_010: case (t)

            1: begin t <= 2; cp[7:0]  <= in;    pc <= pc + 1; end
            2: begin t <= 3; cp[15:8] <= in;    we <= 1; alt <= 1; out <= hl[ 7:0]; end
            3: begin t <= 4; cp <= cp + 1;  we <= 1; alt <= 1; out <= hl[15:8]; end
            4: begin t <= 0; pc <= pc + 1; end

        endcase

        // 5 LD HL, (**)
        8'b00_101_010: case (t)

            1: begin t <= 2; pc <= pc + 1; cp[ 7:0] <= in; end
            2: begin t <= 3; pc <= pc + 1; cp[15:8] <= in; alt <= 1; end
            3: begin t <= 4; _n <= `REG_L; _b <= 1; _l <= in; alt <= 1; cp <= cp + 1; end
            4: begin t <= 0; _n <= `REG_H; _b <= 1; _l <= in; end

        endcase

        // 4 LD (**), A
        8'b00_110_010: case (t)

            1: begin t <= 2; cp[ 7:0] <= in; pc <= pc + 1; end
            2: begin t <= 3; cp[15:8] <= in; we <= 1; alt <= 1; out <= a[7:0]; end
            3: begin t <= 0; pc <= pc + 1; end

        endcase

        // 4 LD A, (**)
        8'b00_111_010: case (t)

            1: begin t <= 2; pc <= pc + 1; cp[ 7:0] <= in; end
            2: begin t <= 3; pc <= pc + 1; cp[15:8] <= in; alt <= 1; end
            3: begin t <= 0; _b <= 1; _n <= `REG_A; _l <= in; end

        endcase

        // 2 INC r16
        8'b00_xx0_011: case (t)

            0: begin _n <= opcode[5:4]; end
            1: begin t <= 0; {_u, _l} <= _r16 + 1; _w <= 1; end

        endcase

        // 2 DEC r16
        8'b00_xx1_011: case (t)

            0: begin _n <= opcode[5:4]; end
            1: begin t <= 0; {_u, _l} <= _r16 - 1; _w <= 1; end

        endcase

        // 4 INC r8
        // 4 DEC r8
        8'b00_xxx_10x: case (t)

            0: begin alt <= 1; _n <= opcode[5:3]; cp <= hl;  end
            1: begin t <= 2; op1 <= reg_hl ? in : _r8; op2 <= 1; alu_m <= opcode[0] ? ALU_SUB : ALU_ADD; end
            2: begin t <= we ? 3 : 0; we <= reg_hl; _b <= ~reg_hl; _f <= alu_f; _l <= alu_r; out <= alu_r; fw <= 1'b1; alt <= 1'b1; end
            3: begin t <= 0; end

        endcase

        // 3 LD r, i8
        8'b00_xxx_110: case (t)

            0: begin _n <= opcode[5:3]; cp <= hl; end
            1: begin t <= 2; pc <= pc + 1; _b <= ~reg_hl; we <= reg_hl; _l <= in; out <= in; alt <= 1; end
            2: begin t <= 0; end

        endcase

        // 2 RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
        8'b00_xxx_111: case (t)

            0: begin op1 <= a; alu_m <= {1'b1, opcode[5:3]}; end
            1: begin t <= 0; _b <= 1; _l <= alu_r; _n <= `REG_A; fw <= 1'b1; _f <= alu_f; end

        endcase

        // 4 LD r, r
        8'b01_110_110: halt <= 1;
        8'b01_xxx_xxx: case (t)

            0: begin _n <= opcode[2:0]; alt <= 1; cp <= hl; end
            1: begin t <= 2; _l <=  reg_hl ? in : _r8;   _n <= opcode[5:3]; end
            2: begin t <= 3; _b <= ~reg_hl; we <= reg_hl; out <= _l; alt <= 1; end
            3: begin t <= 0; end

        endcase

        // 3 <alu> A, r
        8'b10_xxx_xxx: case (t)

            0: begin op1 <= a; _n <= opcode[2:0]; alt <= 1; cp <= hl; end
            1: begin t <= 2; op2   <= reg_hl ? in : _r8; alu_m <= opcode[5:3]; end
            2: begin t <= 0; _b <= (alu_m != 3'b111); _n <= `REG_A; _l <= alu_r; fw <= 1'b1; _f <= alu_f; end

        endcase

        // 2/3 RET c | RET
        8'b11_001_001,
        8'b11_xxx_000: case (t)

            0: begin t <= ccc; alt <= ccc; cp <= sp; end
            1: begin t <= 2; pc[ 7:0] <= in; alt   <= 1; cp <= cp + 1; end
            2: begin t <= 0; pc[15:8] <= in; _w <= 1; {_u, _l} <= cp + 1; _n <= `REG_SP; end

        endcase

        // 4 POP r16
        8'b11_xx0_001: case (t)

            0: begin cp <= sp; alt <= 1; end
            1: begin t <= 2; cp <= cp + 1; _l <= in; alt <= 1;  end
            2: begin t <= 3; cp <= cp + 1; _u <= in;

                 if (opcode[5:4] == 2'b11) // POP AF
                      begin _n <= `REG_A;      _b <= 1; _l <= in; fw <= 1'b1; _f <= _l; end
                 else begin _n <= opcode[5:4]; _w <= 1; end

            end
            3: begin t <= 0; _n <= `REG_SP; _w <= 1; {_u, _l} <= cp; end

        endcase

        // 1 JP (HL)
        8'b11_101_001: case (t)

            0: begin pc <= hl; t <= 0; end

        endcase

        // 1 LD SP, HL
        8'b11_111_001: case (t)

            0: begin t <= 0; _n <= `REG_SP; _w <= 1; {_u, _l} <= hl; end

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

            1: begin t <= 0; pc <= pc + 1; pin_pa <= in; pin_po <= a; pin_pw <= 1; end

        endcase

        // 3 IN  A, (*)
        8'b11_011_011: case (t)

            1: begin t <= 2; pc <= pc + 1; pin_pa <= in; end
            2: begin t <= 0; _l <= pin_pi; _b <= 1; _n <= `REG_A; end

        endcase

        // 5 EX (SP), HL
        8'b11_100_011: case (t)

            0: begin alt <= 1; cp <= sp; end
            1: begin alt <= 1; t <= 2; _l <= in; out <= hl[7:0]; we <= 1; end
            2: begin alt <= 1; t <= 3; cp <= cp + 1; end
            3: begin alt <= 1; t <= 4; _u <= in; _w <= 1; _n <= `REG_HL; out <= hl[15:8]; we <= 1; end
            4: begin t <= 0; end

        endcase

        // 1 EX DE, HL
        8'b11_101_011: case (t)

            0: begin t <= 0; ex_de_hl <= 1; end

        endcase

        // 1 DI, EI
        8'b11_11x_011: case (t)

            0: begin t <= 0; ei_ <= opcode[3]; end

        endcase

        // 3/6 CALL c, **
        8'b11_001_101,
        8'b11_xxx_100: case (t)

            1: begin t <= 2; pc <= pc + 1; _l <= in; end
            2: begin         pc <= pc + 1; _u <= in; cp <= sp;
                     t <= ccc ? 3 : 0; end
            3: begin t <= 4; out <= pc[15:8]; we <= 1; alt <= 1; cp <= cp - 1; end
            4: begin t <= 5; out <= pc[ 7:0]; we <= 1; alt <= 1; cp <= cp - 1; end
            5: begin t <= 0; _w <= 1; _n <= `REG_SP; pc <= {_u, _l}; {_u, _l} <= cp; end

        endcase

        // 4 PUSH r16
        8'b11_xx0_101: case (t)

            0: begin _n <= opcode[5:4]; cp <= sp; end
            1: begin t <= 2; alt <= 1; out <= (_n == 2'b11) ? a : _r16[15:8]; we <= 1; cp <= cp - 1; end
            2: begin t <= 3; alt <= 1; out <= (_n == 2'b11) ? f : _r16[ 7:0]; we <= 1; cp <= cp - 1; end
            3: begin t <= 0; _w <= 1; _n <= `REG_SP; {_u, _l} <= cp; end

        endcase

        // 3 <alu> A, i8
        8'b11_xxx_110: case (t)

            0: begin alu_m <= opcode[5:3]; op1 <= a; end
            1: begin t <= 2; pc <= pc + 1; op2 <= in; end
            2: begin t <= 0; fw <= 1'b1; _f <= alu_f; _n <= `REG_A; _b <= (alu_m != 3'b111); _l <= alu_r; end

        endcase

        // 4 RST #
        8'b11_xxx_111: case (t)

            0: begin cp <= sp; end
            1: begin t <= 2; out <= pc[15:8]; we <= 1; cp <= cp - 1; alt <= 1; end
            2: begin t <= 3; out <= pc[ 7:0]; we <= 1; cp <= cp - 1; alt <= 1; end
            3: begin t <= 0; _w <= 1; _n <= `REG_SP; {_u, _l} <= cp; pc <= {opcode[5:3], 3'b000}; end

        endcase

        endcase

    end

end

// -----------------------------------------------------------------------------
// Арифметико-логическое устройство
// -----------------------------------------------------------------------------

wire flag_carry =   alu_r[8];
wire flag_sign  =   alu_r[7];
wire flag_zero  = ~|alu_r[7:0];
wire flag_prty  = ~^alu_r[7:0];
wire flag_aux   =  op1[4] ^ op2[4] ^ alu_r[4];
wire flag_add   = (op1[7] == op2[7]) && (op1[7] != alu_r[7]);
wire flag_sub   = (op1[7] != op2[7]) && (op1[7] != alu_r[7]);

always @* begin

    // Определение результата
    case (alu_m)
    ALU_ADD: alu_r = op1 + op2;
    ALU_ADC: alu_r = op1 + op2 + f[`CF];
    ALU_SUB: alu_r = op1 - op2;
    ALU_SBC: alu_r = op1 - op2 - f[`CF];
    ALU_AND: alu_r = op1 & op2;
    ALU_XOR: alu_r = op1 ^ op2;
    ALU_OR:  alu_r = op1 | op2;
    ALU_CP:  alu_r = op1 - op2;
    ALU_RLC: alu_r = {op1[6:0], op1[7]};
    ALU_RRC: alu_r = {op1[0], op1[7:1]};
    ALU_RL:  alu_r = {op1[6:0], f[`CF]};
    ALU_RR:  alu_r = {f[`CF], op1[7:1]};
    ALU_CPL: alu_r = ~a;
    ALU_SCF: alu_r = a;
    ALU_RLC: alu_r = {op1[6:0], op1[7]};
    ALU_RRC: alu_r = {op1[0],   op1[7:1]};
    ALU_RL:  alu_r = {op1[6:0], f[`CF]};
    ALU_RR:  alu_r = {f[`CF], op1[7:1]};
    ALU_DAA: alu_r = (f[`NF]) ? op1 - ((f[`AF] | (op1[3:0] > 4'h9)) ? 8'h06 : 0) - ((f[`CF] | (op1[7:0] > 8'h99)) ? 8'h60 : 0) :
                                op1 + ((f[`AF] | (op1[3:0] > 4'h9)) ? 8'h06 : 0) + ((f[`CF] | (op1[7:0] > 8'h99)) ? 8'h60 : 0);
    endcase

    // Определение флагов
    case (alu_m)
    ALU_ADD,
    ALU_ADC: alu_f = {flag_sign, flag_zero, 1'b0, flag_aux, 1'b0, flag_add, 1'b0, flag_carry};
    ALU_SUB,
    ALU_CP,
    ALU_SBC: alu_f = {flag_sign, flag_zero, 1'b0, flag_aux, 1'b0, flag_sub, 1'b1, flag_carry};
    ALU_OR,
    ALU_XOR,
    ALU_AND: alu_f = {flag_sign, flag_zero, 3'b000, flag_prty, 2'b00};
    ALU_DAA: alu_f = {flag_sign, flag_zero, alu_r[5], a[4] ^ alu_r[4], alu_r[3], flag_prty, f[`NF], f[`CF] | (a > 8'h99)};
    ALU_CPL: alu_f = {f[`SF], f[`ZF], 3'b010, f[`PF], 1'b1, f[`CF]};
    ALU_SCF: alu_f = {f[`SF], f[`ZF], 1'b0, f[`AF], 1'b0, f[`PF], 2'b11};
    ALU_CCF: alu_f = {f[`SF], f[`ZF], 1'b0, f[`AF], 1'b0, f[`PF], 1'b1, f[`CF] ^ 1'b1};
    ALU_RL,
    ALU_RLC: alu_f = {flag_sign, flag_zero, 3'b000, flag_prty, 1'b1, op1[7]};
    ALU_RR,
    ALU_RRC: alu_f = {flag_sign, flag_zero, 3'b000, flag_prty, 1'b1, op1[0]};
    endcase

end

// ---------------------------------------------------------------------
// Работа с регистрами
// ---------------------------------------------------------------------

// Чтение
always @* begin

    _r8  = 8'h00;
    _r16 = 16'h0000;

    case (_n)
    3'h0: _r8 = bc[15:8]; 3'h1: _r8 = bc[ 7:0];
    3'h2: _r8 = de[15:8]; 3'h3: _r8 = de[ 7:0];
    3'h4: _r8 = hl[15:8]; 3'h5: _r8 = hl[ 7:0];
    3'h6: _r8 = f;        3'h7: _r8 = a;
    endcase

    case (_n)
    3'h0: _r16 = bc;
    3'h1: _r16 = de;
    3'h2: _r16 = hl;
    3'h3: _r16 = sp;
    3'h4: _r16 = {a, f};
    endcase

end

// Запись в регистры
always @(negedge clock)
begin

    if   (ex_de_hl) begin de <= hl; hl <= de; end
    else if (_w) begin

        case (_n)

            3'h0: bc <= {_u, _l};
            3'h1: de <= {_u, _l};
            3'h2: hl <= {_u, _l};
            3'h3: sp <= {_u, _l};

        endcase

    end
    else if (_b) begin

        case (_n)

            3'h0: bc[15:8] <= _l;
            3'h1: bc[ 7:0] <= _l;
            3'h2: de[15:8] <= _l;
            3'h3: de[ 7:0] <= _l;
            3'h4: hl[15:8] <= _l;
            3'h5: hl[ 7:0] <= _l;
            3'h7: a <= _l;

        endcase

    end

    // Сохранение флагов
    if (fw) f <= _f;

end

endmodule
