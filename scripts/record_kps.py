import keyboard
import time

def save_key_press(f):
    def key_press(key):
        print(key.name)
        print(key.name, file=f)

    return key_press

with open('raw', 'w') as f:
    keyboard.on_press(save_key_press(f))

    while True:
        time.sleep(1)
