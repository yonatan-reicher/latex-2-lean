class A:
    @classmethod
    def foo(cls, x):
        print(f"Running A's foo with {cls=} {x=}")

class B(A):
    @classmethod
    def foo(cls, x):
        print(f"Running B's foo with {cls=} {x=}")
        super().foo(x)

print("Control group:")
a = A()
a.foo(1)
print("Experimental group:")
b = B()
b.foo(2)
