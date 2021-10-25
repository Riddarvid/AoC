from intcode_computer.IntcodeComputer import IntcodeComputer

def part1(ints):
    computer = IntcodeComputer(ints)
    computer.memory[1] = 12
    computer.memory[2] = 2
    computer.run()
    return computer.memory[0]

def part2(ints):
    for noun in range(0, 100):
        for verb in range(0, 100):
            computer = IntcodeComputer(ints)
            computer.memory[1] = noun
            computer.memory[2] = verb
            computer.run()
            if computer.memory[0] == 19690720:
                return 100 * noun + verb
    raise AttributeError("No solution found")

def run():
    f = open('input', 'r')
    line = f.readline()
    lines = line.split(',')
    ints = [int(i) for i in lines]
    print(part1(ints))
    print(part2(ints))

run()