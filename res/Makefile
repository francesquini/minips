# Adaptado do Makefile escrito por Eduardo Renesto

CC = mipsel-linux-gnu-gcc
ARTIFACTS := 13.arit.data 13.arit.text 13.arit.rodata 13.arit.elf \
           15.pi.data 15.pi.text 15.pi.rodata 15.pi.elf         \
		   16.automato.data 16.automato.text 16.automato.rodata 16.automato.elf

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

%.o: %.c
	${CC} ${CFLAGS} $^ -c -o $@

%.elf: %.o
	${CC} ${CFLAGS} -T linker_script.ld $^ -o $@

%.text: %.elf
	mipsel-linux-gnu-objcopy -O binary --only-section=.text $^ $@

%.data: %.elf
	mipsel-linux-gnu-objcopy -O binary --only-section=.data $^ $@

%.rodata: %.elf
	mipsel-linux-gnu-objcopy -O binary --only-section=.rodata $^ $@

all: ${ARTIFACTS}

clean:
	rm *.o -f
	rm *.elf -f

.PRECIOUS: %.o

.PHONY: clean
