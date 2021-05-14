from random import random, randint, choice

stations = set()
for station in range(20000):
    stations.add(f"{round(19.2+random(), 3)},{round(49.5+random(),3)}")

slist = list(stations)
for _ in range(500000):
    print(f"05-05-2017,{randint(0,23)}:{randint(0,59)},{choice(slist)},{randint(0,100)}")

# 31 * 23 * 59 = 42067
for d in range(31):
    for h in range(23):
        for m in range(59):
            print(f"{d}-05-2017,{h}:{m},{20},{50},{randint(0,100)}")
