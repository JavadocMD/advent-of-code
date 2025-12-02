package aoc2023

def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

def lcm(a: Long, b: Long) = (a * b).abs / gcd(a, b)
