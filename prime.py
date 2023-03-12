# This code will generate a lot of CPU usage by continuously computing prime numbers

def is_prime(n):
    if n <= 1:
        return False
    for i in range(2, n):
        if n % i == 0:
            return False
    return True

n = 1
while True:
    if is_prime(n):
        print(n)
    n += 1
