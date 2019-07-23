
import sys


class StackVM(object):
	def __init__(self, routines, inputs):
		"""Routines should be a map {name: list of chars}.
		Inputs should be list of int or char."""
		self.routines = routines
		self.inputs = inputs
		self.data_stack = []
		self.call_stack = [('!', 0)]
		self.steps = 0

	@property
	def done(self):
		return not self.call_stack

	def format(self):
		if not self.done:
			name, offset = self.call_stack[-1]
			routine = self.routines[name]
		return '\n'.join([
			"Step {}".format(self.steps),
			"Call stack: {}".format(' '.join([c for c, i in self.call_stack])),
			"Data stack: {}".format(' '.join(map(str, self.data_stack))),
			"Done" if self.done else "Current routine: {}\x1b[31;1m{}\x1b[m{}".format(
				routine[:offset],
				routine[offset:offset+1],
				routine[offset+1:],
			),
		])

	def step(self):
		name, offset = self.call_stack[-1]
		routine = self.routines[name]
		# implicit return at end of functions still acts as an instruction for timing
		instr = routine[offset] if offset < len(routine) else 'E'

		# advance instr pointer
		self.call_stack[-1] = name, offset + 1

		# shortcuts for brevity
		c = self.call_stack
		d = self.data_stack
		push = d.append
		pop = d.pop
		ret = lambda: c.pop()
		def call(name):
			if name not in self.routines:
				raise ValueError("Tried to call on bad value {!r}".format(name))
			c.append((name, 0))

		out = None

		if instr == 'I':
			push(self.inputs.pop(0))
		elif instr == 'O':
			out = pop()
		elif instr == 'C':
			call(pop())
		elif instr == 'T':
			ret()
			call(pop())
		elif instr == 'E':
			ret()
		elif instr == 'Z':
			if pop() == 0: # note: ignores (doesn't return) non-numeric values
				ret()
		elif instr == 'D':
			pop()
		elif instr == 'R':
			n = pop()
			x = pop()
			d.insert(-n, x) # note: if n > stack size, puts at bottom
		elif instr == 'U':
			x = pop()
			push(x)
			push(x)
		elif instr == '+':
			push(pop() + pop())
		elif instr == '-':
			x = pop()
			y = pop()
			push(y - x)
		elif instr == '*':
			push(pop() * pop())
		elif instr == '/':
			d = pop()
			n = pop()
			push(n / d)
			push(n % d)
		elif instr == '~':
			push(-pop())
		# TODO: conditional operator?
		# TODO: explode/collapse (x y <-> 10 * x + y)
		elif instr.isdigit():
			push(int(instr))
		else: # routine name
			push(instr)

		self.steps += 1
		return out


def main(*inputs):
	"""Takes a stack program on stdin in format:
		NAME CHARS
	for each routine. The main routine is '!'.
	Comments start with ;
	The inputs are the sequence of args and may be numbers or routine names.

	Will print the full vm state at each step.
	"""
	inputs = [int(i) if i.isdigit() else i for i in inputs]
	routines = {}
	for line in sys.stdin:
		line = line.strip()
		if not line: continue
		if line.startswith(';'): continue
		name, chars = line.split(' ', 1)
		routines[name] = chars.strip()
	vm = StackVM(routines, inputs)
	print vm.format()
	print
	while not vm.done:
		output = vm.step()
		if output is not None:
			print "Output: {}".format(output)
			print
		print vm.format()
		print


if __name__ == '__main__':
	import argh
	argh.dispatch_command(main)
