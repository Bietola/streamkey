from pathlib import Path
import sys
import keyboard
import time
import fire

def save_key_press(f):
    def do_it(key):
        print(key)
        print("hello there")
        sys.stdout.flush()

    return do_it

def record_kps():
    keyboard.hook(print)

    while True:
        time.sleep(1)

if __name__ == '__main__':
    fire.Fire(record_kps)
