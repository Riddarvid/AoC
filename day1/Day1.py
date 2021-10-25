def get_fuel_1(mass):
    return mass // 3 - 2

def get_total_fuel_1(mass_list):
    fuel = 0
    for mass in mass_list:
        fuel += get_fuel_1(mass)
    return fuel

def get_fuel_2(mass):
    if mass <= 0:
        return 0
    fuel = get_fuel_1(mass)
    return fuel + get_fuel_2(fuel)

def get_total_fuel_2(mass_list):
    fuel = 0
    for mass in mass_list:
        fuel += get_fuel_2(mass)
    return fuel

def run():
    f = open('input', 'r')
    lines = f.readlines()
    ints = [int(i) for i in lines]
    print(get_total_fuel_1(ints))
    print(get_total_fuel_2(ints))

run()