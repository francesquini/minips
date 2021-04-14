# Adaptado do Makefile escrito por Eduardo Renesto
CC_NATIVE = gcc
CC = mipsel-linux-gnu-gcc
ARTIFACTS := 13.arit.data 13.arit.text 13.arit.rodata 13.arit.elf \
			 15.pi.data 15.pi.text 15.pi.rodata 15.pi.elf         \
			 16.automato.data 16.automato.text 16.automato.rodata 16.automato.elf \
			 17.rng.data 17.rng.text 17.rng.rodata 17.rng.elf \
			 18.naive_dgemm.data 18.naive_dgemm.text 18.naive_dgemm.rodata 18.naive_dgemm.elf \
			 19.regular_dgemm.data 19.regular_dgemm.text 19.regular_dgemm.rodata 19.regular_dgemm.elf \
			 20.blocking_dgemm.data 20.blocking_dgemm.text 20.blocking_dgemm.rodata 20.blocking_dgemm.elf \
			 21.mandelbrot.data 21.mandelbrot.text 21.mandelbrot.rodata 21.mandelbrot.elf

# - arquitetura mips e com símbolos de debug
# - com otimizações para acelerar a execução
# - standalone, estático, sem código relocalizável, ...
# - sem exceções de FP e evita promoção de float->double
# - Warnings completos e tratados como erros
CFLAGS := -mips1 -g \
		  -O2 -fno-lto \
		  -nostdlib -mno-shared -ffreestanding -static -fno-pie -fno-pic -fno-toplevel-reorder \
		  -mno-fp-exceptions -Wdouble-promotion \
		  -Wall -Werror
C_NATIVE_FLAGS := -g -O2 -Wall

%.o: %.c
	${CC} ${CFLAGS} $^ -c -o $@
	${CC_NATIVE} ${C_NATIVE_FLAGS} $^ -c -o $@.native

%.elf: %.o
	${CC} ${CFLAGS} -T linker_script.ld $^ -o $@
	${CC_NATIVE} ${C_NATIVE_FLAGS} $^.native -o $@.native

%.text: %.elf
	mipsel-linux-gnu-objcopy -O binary --only-section=.text $^ $@

%.data: %.elf
	mipsel-linux-gnu-objcopy -O binary --only-section=.data $^ $@

%.rodata: %.elf
	mipsel-linux-gnu-objcopy -O binary --only-section=.rodata $^ $@

all: ${ARTIFACTS}

clean:
	rm *.o -f
	rm *.o.native -f
	rm *.elf -f
	rm *.elf.native -f

.PRECIOUS: %.o %.o.native

.PHONY: clean
