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

`define REG_BC      0
`define REG_DE      1
`define REG_HL      2
`define REG_SP      3

initial begin

    we = 0;
    out   = 0;
    pin_pa  = 0;
    pin_po  = 0;

end

// Указатель на необходимые данные
assign address = alt ? cursor : pc;

// Управляющие регистры
reg  [ 3:0] t       = 0;        // Это t-state
reg  [ 2:0] m       = 0;        // Это m-state для префиксов
reg         halt    = 0;        // Процессор остановлен
reg         ei      = 0;        // Enabled Interrupt
reg         ei_     = 0;        // Это необходимо для EI+RET конструкции
reg  [15:0] cursor  = 0;
reg         alt   = 1'b0;     // =0 pc  =1 cursor

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
reg         reg_b = 1'b0;       // Сигнал на запись 8 битного регистра
reg         reg_w = 1'b0;       // Сигнал на запись 16 битного регистра (reg_u:reg_v)
reg  [ 2:0] reg_n = 3'h0;       // Номер регистра
reg  [ 7:0] reg_l = 8'h00;      // Что писать
reg  [ 7:0] reg_u = 8'h00;      // Что писать
reg  [ 7:0] reg_f = 8'h00;      // Сохранение флага
reg  [ 7:0] reg_r8;             // reg_r8  = regs8 [ reg_n ]
reg  [15:0] reg_r16;            // reg_r16 = regs16[ reg_n ]
reg  [ 1:0] reg_ldir;           // 1=DE++, HL++; 2=DE--, HL--;
reg         fw;                 // Писать флаги
reg         ex_de_hl;

// Определение условий
wire        reg_hl  = (reg_n == 3'b110);
wire [15:0] signext = {{8{in[7]}}, in[7:0]};
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
    reg_b    <= 1'b0;
    reg_w    <= 1'b0;
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

                reg_b <= 1;
                reg_n <= `REG_B;
                reg_l <= bc[15:8] - 1;

                if (bc[15:8] == 1) pc <= pc + 2;

            end
            1: begin t <= 0; pc <= pc + 1 + signext; end

        endcase

        // 2 JR *
        8'b00_011_000: case (t)

            1: begin t <= 0; pc <= pc + 1 + signext; end

        endcase

        // 1|2 JR cc, *
        8'b00_1xx_000: case (t)

            0: begin if (!cc[opcode[4:3]]) begin t <= 0; pc <= pc + 2; end end
            1: begin t <= 0; pc <= pc + 1 + signext; end

        endcase

        // 3 LD r, i16
        8'b00_xx0_001: case (t)

            0: begin reg_n <= opcode[5:4]; end
            1: begin pc <= pc + 1; t <= 2; reg_l <= in; end
            2: begin pc <= pc + 1; t <= 0; reg_u <= in; reg_w <= 1'b1; end

        endcase

        // 4 ADD HL, r
        8'b00_xx1_001: case (t)

            0: begin reg_n <= {opcode[5:4], 1'b1}; end
            1: begin

                t           <= 2;
                alu_m       <= ALU_ADD;
                reg_n       <= {opcode[5:4], 1'b0};
                reg_f       <= f;
                op1         <= hl[7:0];
                op2         <= reg_r8;

            end
            2: begin

                t           <= 3;
                alu_m       <= ALU_ADC;
                op1         <= hl[15:8];
                op2         <= reg_r8;
                reg_n       <= `REG_L;
                reg_b       <= 1'b1;
                reg_l       <= alu_r[7:0];
                reg_f[0]    <= alu_f[`CF];
                fw          <= 1'b1;

            end
            3: begin

                t           <= 0;
                reg_n       <= `REG_H;
                reg_l       <= alu_r[7:0];
                reg_b       <= 1'b1;
                fw          <= 1'b1;
                reg_f[`AF]  <= alu_f[`AF];
                reg_f[`CF]  <= alu_f[`CF];
                reg_f[`SF]  <= alu_f[`SF];

            end

        endcase

        // 2 LD (BC|DE), A
        8'b00_0x0_010: case (t)

            0: begin alt <= 1; cursor <= opcode[4] ? de : bc; out <= a; we <= 1; end
            1: begin alt <= 0; t <= 0; end

        endcase

        // 2 LD A, (BC|DE)
        8'b00_0x1_010: case (t)

            0: begin alt <= 1; cursor <= opcode[4] ? de : bc;  end
            1: begin t <= 0; reg_n <= `REG_A; reg_b <= 1; reg_l <= in; end

        endcase

        // 4 LD (**), HL
        8'b00_100_010: case (t)

            1: begin t <= 2; cursor[7:0]  <= in;    pc <= pc + 1; end
            2: begin t <= 3; cursor[15:8] <= in;    we <= 1; alt <= 1; out <= hl[ 7:0]; end
            3: begin t <= 4; cursor <= cursor + 1;  we <= 1; alt <= 1; out <= hl[15:8]; end
            4: begin t <= 0; pc <= pc + 1; end

        endcase

        // 5 LD HL, (**)
        8'b00_101_010: case (t)

            1: begin t <= 2; pc <= pc + 1; cursor[ 7:0] <= in; end
            2: begin t <= 3; pc <= pc + 1; cursor[15:8] <= in; alt <= 1; end
            3: begin t <= 4; reg_n <= `REG_L; reg_b <= 1; reg_l <= in; alt <= 1; cursor <= cursor + 1; end
            4: begin t <= 0; reg_n <= `REG_H; reg_b <= 1; reg_l <= in; end

        endcase

        // 4 LD (**), A
        8'b00_110_010: case (t)

            1: begin t <= 2; cursor[ 7:0] <= in; pc <= pc + 1; end
            2: begin t <= 3; cursor[15:8] <= in; we <= 1; alt <= 1; out <= a[7:0]; end
            3: begin t <= 0; pc <= pc + 1; end

        endcase

        // 4 LD A, (**)
        8'b00_111_010: case (t)

            1: begin t <= 2; pc <= pc + 1; cursor[ 7:0] <= in; end
            2: begin t <= 3; pc <= pc + 1; cursor[15:8] <= in; alt <= 1; end
            3: begin t <= 0; reg_b <= 1; reg_n <= `REG_A; reg_l <= in; end

        endcase

        // 2 INC r16
        8'b00_xx0_011: case (t)

            0: begin reg_n <= opcode[5:4]; end
            1: begin t <= 0; {reg_u, reg_l} <= reg_r16 + 1; reg_w <= 1; end

        endcase

        // 2 DEC r16
        8'b00_xx1_011: case (t)

            0: begin reg_n <= opcode[5:4]; end
            1: begin t <= 0; {reg_u, reg_l} <= reg_r16 - 1; reg_w <= 1; end

        endcase

        // 4 INC r8
        // 4 DEC r8
        8'b00_xxx_10x: case (t)

            0: begin alt <= 1; reg_n <= opcode[5:3]; cursor <= hl;  end
            1: begin t <= 2; op1 <= reg_hl ? in : reg_r8; op2 <= 1; alu_m <= opcode[0] ? ALU_SUB : ALU_ADD; end
            2: begin t <= we ? 3 : 0; we <= reg_hl; reg_b <= ~reg_hl; reg_f <= alu_f; reg_l <= alu_r; out <= alu_r; fw <= 1'b1; alt <= 1'b1; end
            3: begin t <= 0; end

        endcase

        // 3 LD r, i8
        8'b00_xxx_110: case (t)

            0: begin reg_n <= opcode[5:3]; cursor <= hl; end
            1: begin t <= 2; pc <= pc + 1; reg_b <= ~reg_hl; we <= reg_hl; reg_l <= in; out <= in; alt <= 1; end
            2: begin t <= 0; end

        endcase

        // 2 RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, CCF
        8'b00_xxx_111: case (t)

            0: begin op1 <= a; alu_m <= {1'b1, opcode[5:3]}; end
            1: begin t <= 0; reg_b <= 1; reg_l <= alu_r; reg_n <= `REG_A; fw <= 1'b1; reg_f <= alu_f; end

        endcase

        // 4 LD r, r
        8'b01_110_110: halt <= 1;
        8'b01_xxx_xxx: case (t)

            0: begin reg_n <= opcode[2:0]; alt <= 1; cursor <= hl; end
            1: begin t <= 2; reg_l <=  reg_hl ? in : reg_r8;   reg_n <= opcode[5:3]; end
            2: begin t <= 3; reg_b <= ~reg_hl; we <= reg_hl; out <= reg_l; alt <= 1; end
            3: begin t <= 0; end

        endcase

        // 3 <alu> A, r
        8'b10_xxx_xxx: case (t)

            0: begin op1 <= a; reg_n <= opcode[2:0]; alt <= 1; cursor <= hl; end
            1: begin t <= 2; op2   <= reg_hl ? in : reg_r8; alu_m <= opcode[5:3]; end
            2: begin t <= 0; reg_b <= (alu_m != 3'b111); reg_n <= `REG_A; reg_l <= alu_r; fw <= 1'b1; reg_f <= alu_f; end

        endcase

        // 2/3 RET c | RET
        8'b11_001_001,
        8'b11_xxx_000: case (t)

            0: begin t <= ccc; alt <= ccc; cursor <= sp; end
            1: begin t <= 2; pc[ 7:0] <= in; alt   <= 1; cursor <= cursor + 1; end
            2: begin t <= 0; pc[15:8] <= in; reg_w <= 1; {reg_u, reg_l} <= cursor + 1; reg_n <= `REG_SP; end

        endcase

        // 4 POP r16
        8'b11_xx0_001: case (t)

            0: begin cursor <= sp; alt <= 1; end
            1: begin t <= 2; cursor <= cursor + 1; reg_l <= in; alt <= 1;  end
            2: begin t <= 3; cursor <= cursor + 1; reg_u <= in;

                 if (opcode[5:4] == 2'b11) // POP AF
                      begin reg_n <= `REG_A;      reg_b <= 1; reg_l <= in; fw <= 1'b1; reg_f <= reg_l; end
                 else begin reg_n <= opcode[5:4]; reg_w <= 1; end

            end
            3: begin t <= 0; reg_n <= `REG_SP; reg_w <= 1; {reg_u, reg_l} <= cursor; end

        endcase

        // 1 JP (HL)
        8'b11_101_001: case (t)

            0: begin pc <= hl; t <= 0; end

        endcase

        // 1 LD SP, HL
        8'b11_111_001: case (t)

            0: begin t <= 0; reg_n <= `REG_SP; reg_w <= 1; {reg_u, reg_l} <= hl; end

        endcase

        // 4 JP c, ** | JP **
        8'b11_000_011,
        8'b11_xxx_010: case (t)

            1: begin t <= 2; pc <= pc + 1'b1; reg_l <= in; end
            2: begin t <= 3; pc <= pc + 1'b1; reg_u <= in; end
            3: begin t <= 0; if (ccc) pc <= {reg_u, reg_l}; end

        endcase

        // 2 OUT (*), A
        8'b11_010_011: case (t)

            1: begin t <= 0; pc <= pc + 1; pin_pa <= in; pin_po <= a; pin_pw <= 1; end

        endcase

        // 3 IN  A, (*)
        8'b11_011_011: case (t)

            1: begin t <= 2; pc <= pc + 1; pin_pa <= in; end
            2: begin t <= 0; reg_l <= pin_pi; reg_b <= 1; reg_n <= `REG_A; end

        endcase

        // 5 EX (SP), HL
        8'b11_100_011: case (t)

            0: begin alt <= 1; cursor <= sp; end
            1: begin alt <= 1; t <= 2; reg_l <= in; out <= hl[7:0]; we <= 1; end
            2: begin alt <= 1; t <= 3; cursor <= cursor + 1; end
            3: begin alt <= 1; t <= 4; reg_u <= in; reg_w <= 1; reg_n <= `REG_HL; out <= hl[15:8]; we <= 1; end
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

            1: begin t <= 2; pc <= pc + 1; reg_l <= in; end
            2: begin         pc <= pc + 1; reg_u <= in; cursor <= sp;
                     t <= ccc ? 3 : 0; end
            3: begin t <= 4; out <= pc[15:8]; we <= 1; alt <= 1; cursor <= cursor - 1; end
            4: begin t <= 5; out <= pc[ 7:0]; we <= 1; alt <= 1; cursor <= cursor - 1; end
            5: begin t <= 0; reg_w <= 1; reg_n <= `REG_SP; pc <= {reg_u, reg_l}; {reg_u, reg_l} <= cursor; end

        endcase

        // 4 PUSH r16
        8'b11_xx0_101: case (t)

            0: begin reg_n <= opcode[5:4]; cursor <= sp; end
            1: begin t <= 2; alt <= 1; out <= (reg_n == 2'b11) ? a : reg_r16[15:8]; we <= 1; cursor <= cursor - 1; end
            2: begin t <= 3; alt <= 1; out <= (reg_n == 2'b11) ? f : reg_r16[ 7:0]; we <= 1; cursor <= cursor - 1; end
            3: begin t <= 0; reg_w <= 1; reg_n <= `REG_SP; {reg_u, reg_l} <= cursor; end

        endcase

        // 3 <alu> A, i8
        8'b11_xxx_110: case (t)

            0: begin alu_m <= opcode[5:3]; op1 <= a; end
            1: begin t <= 2; pc <= pc + 1; op2 <= in; end
            2: begin t <= 0; reg_l <= alu_r; fw <= 1'b1; reg_f <= alu_f; reg_n <= `REG_A; reg_b <= (alu_m != 3'b111); end

        endcase

        // 4 RST #
        8'b11_xxx_111: case (t)

            0: begin cursor <= sp; end
            1: begin t <= 2; out <= pc[15:8]; we <= 1; cursor <= cursor - 1; alt <= 1; end
            2: begin t <= 3; out <= pc[ 7:0]; we <= 1; cursor <= cursor - 1; alt <= 1; end
            3: begin t <= 0; reg_w <= 1; reg_n <= `REG_SP; {reg_u, reg_l} <= cursor; pc <= {opcode[5:3], 3'b000}; end

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

    reg_r8  = 8'h00;
    reg_r16 = 16'h0000;

    case (reg_n)
    3'h0: reg_r8 = bc[15:8]; 3'h1: reg_r8 = bc[ 7:0];
    3'h2: reg_r8 = de[15:8]; 3'h3: reg_r8 = de[ 7:0];
    3'h4: reg_r8 = hl[15:8]; 3'h5: reg_r8 = hl[ 7:0];
    3'h6: reg_r8 = f;        3'h7: reg_r8 = a;
    endcase

    case (reg_n)
    3'h0: reg_r16 = bc;
    3'h1: reg_r16 = de;
    3'h2: reg_r16 = hl;
    3'h3: reg_r16 = sp;
    3'h4: reg_r16 = {a, f};
    endcase

end

// Запись в регистры
always @(negedge clock)
begin

    if   (ex_de_hl) begin de <= hl; hl <= de; end
    else if (reg_w) begin

        case (reg_n)

            3'h0: bc <= {reg_u, reg_l};
            3'h1: de <= {reg_u, reg_l};
            3'h2: hl <= {reg_u, reg_l};
            3'h3: sp <= {reg_u, reg_l};

        endcase

    end
    else if (reg_b) begin

        case (reg_n)

            3'h0: bc[15:8] <= reg_l;
            3'h1: bc[ 7:0] <= reg_l;
            3'h2: de[15:8] <= reg_l;
            3'h3: de[ 7:0] <= reg_l;
            3'h4: hl[15:8] <= reg_l;
            3'h5: hl[ 7:0] <= reg_l;
            3'h7: a <= reg_l;

        endcase

    end

    // Сохранение флагов
    if (fw) f <= reg_f;

end

endmodule
