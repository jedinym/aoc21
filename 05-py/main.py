#!/usr/bin/python3
from typing import Generator, Tuple, List, Callable, Type 
from math import sqrt

Point = Tuple[int, int]
Line = Tuple[Point, Point]

def parse_line(line: str) -> Line:
    startstr, endstr = line.split('->')
    start: Point = tuple(map(int, startstr.split(',')))
    end: Point = tuple(map(int, endstr.split(','))) 
    return start, end

def parse_file(filename: str) -> List[Line]:
    lines: List[Line] = []
    with open(filename, 'r') as file:
        for line in file:
            lines.append(parse_line(line))
    return lines


def is_straight(line: Line) -> bool:
    start, end = line
    x1, y1 = start
    x2, y2 = end
    return x1 == x2 or y1 == y2

def max_x(line: Line) -> int:
    start, end = line
    x1, _ = start
    x2, _ = end
    return max(x1, x2)

def max_y(line: Line) -> int:
    start, end = line
    _, y1 = start
    _, y2 = end
    return max(y1, y2)

def line_gen(line: Line) -> Generator[Point, Point, Point]:
    if is_straight(line):
        yield from straight_line_gen(line)
        return 
    yield from diagonal_line_gen(line)


def my_range(a: int, b: int) -> Generator[int, int, int]:
    if a <= b:
        yield from range(a, b + 1)
        return

    for x in range(a, b - 1, -1):
        yield x


def diagonal_line_gen(line: Line) -> Generator[Point, Point, Point]:
    start, end = line
    x1, y1 = start
    x2, y2 = end
    s1, s2 = (x2 - x1, y2 - y1) 

    v1 = -1
    v2 = -1
    if s1 > 0:
        v1 = 1
    
    if s2 > 0:
        v2 = 1
    
    yield start
    current = (x1 + v1, y1 + v2)
    while current != end:
        yield current
        current = (current[0] + v1, current[1] + v2)
    yield end


def straight_line_gen(line: Line) -> Generator[Point, Point, Point]:
    start, end = line
    x1, y1 = start
    x2, y2 = end

    if x1 == x2:
        rng = my_range(y1, y2)
        for y in rng:
            yield x1, y
        return
    if y1 == y2:
        rng = my_range(x1, x2)
        for x in rng:
            yield x, y1
        return


def mark_spots(spots: List[List[int]], lines: List[Line]) -> None:
    for ln in lines:
        line = line_gen(ln)
        for spot in line:
            x, y = spot
            spots[y][x] += 1

def count_intersections(spots: List[List[int]]) -> int:
    count = 0
    for row in spots:
        for spot in row:
            if spot >= 2:
                count += 1
    return count

if __name__ == '__main__':
    lines = parse_file('input.txt')
    # only_straight = filter(is_straight, lines)
    x = max_x(max(lines, key=max_x))
    y = max_y(max(lines, key=max_y))
    arr: List[List[int]] = [[0 for _ in range(0, x + 1)] for _ in range(y + 1)]
    mark_spots(arr, lines)
    print(count_intersections(arr))
