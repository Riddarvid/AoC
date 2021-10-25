class IntcodeComputer:

    def __init__(self, memory):
        self.pc = 0
        self.memory = memory[:]
        self.halted = False
        self.opcode_dict = {
            1: self.add,
            2: self.multiply,
            99: self.halt,
        }

    def run(self):
        while not self.halted:
            opcode = self.memory[self.pc]
            self.pc = self.pc + 1
            self.execute_opcode(opcode)

    def execute_opcode(self, opcode):
        op_function = self.opcode_dict.get(opcode, None)
        if op_function is None:
            raise NotImplementedError(f'Opcode {opcode} not implemented!')
        op_function()

    def add(self):
        operand1_address = self.memory[self.pc]
        self.pc = self.pc + 1
        operand1 = self.memory[operand1_address]
        operand2_address = self.memory[self.pc]
        self.pc = self.pc + 1
        operand2 = self.memory[operand2_address]
        result_address = self.memory[self.pc]
        self.pc = self.pc + 1
        self.memory[result_address] = operand1 + operand2

    def multiply(self):
        operand1_address = self.memory[self.pc]
        self.pc = self.pc + 1
        operand1 = self.memory[operand1_address]
        operand2_address = self.memory[self.pc]
        self.pc = self.pc + 1
        operand2 = self.memory[operand2_address]
        result_address = self.memory[self.pc]
        self.pc = self.pc + 1
        self.memory[result_address] = operand1 * operand2

    def halt(self):
        self.halted = True
