# Name: Kaung Khant Lin
# ID: 6540131, Sec: 542

import numpy as np
import time

N = 10000

arr = np.zeros((N, N))
total = 0

# --- Row-major access ---
start_time = time.time()
for i in range(N):
    for j in range(N):
        total += arr[i, j]
end_time = time.time()
print(f"Row-major access time (via Python loops): {end_time - start_time:.6f} sec")

# --- Column-major access ---
start_time = time.time()
for j in range(N):
    for i in range(N):
        total += arr[i, j]
end_time = time.time()
print(f"Column-major access time (via Python loops): {end_time - start_time:.6f} sec")

# --- Idiomatic NumPy (for comparison) ---
start_time = time.time()
total = np.sum(arr)
end_time = time.time()
print(f"NumPy's optimized np.sum() time: {end_time - start_time:.6f} sec")