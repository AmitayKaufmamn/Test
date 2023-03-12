import time

def is_prime(n):
    if n <= 1:
        return False
    for i in range(2, n):
        if n % i == 0:
            return False
    return True

n = 1
start_time = time.time()
while True:
    if is_prime(n):
        print(n)
    n += 1
    elapsed_time = time.time() - start_time
    if elapsed_time >= 900:  # 900 seconds = 15 minutes
        break

