import tkinter as tk


def Newwinodw():
    window = tk.Tk()
    window.geometry("1000x800+200+180")
    window.resizable(True, True)
    window.title("Analyze Result")

    img = tk.PhotoImage(file="1.png")
    imglabel = tk.Label(window, image=img)
    imglabel.grid(row=1, column=1)
    window.mainloop()


mainapp = Newwinodw()
