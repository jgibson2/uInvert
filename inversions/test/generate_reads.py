from Bio import SeqIO
import sys
import random
for record in SeqIO.parse(sys.argv[1], "fasta"):
    for i in range(0, len(record), 100):
        seq = record[i:i+100]
        start = random.randint(18,25)
        end = random.randint(18, 25)
        inversion = seq[start:len(seq)-end].reverse_complement()
        seq = seq[:start]+inversion+seq[len(seq)-end:]
        print('@{0}_{1}_S{2}_E{3}'.format(record.id, i, start, len(seq)-end))
        print(seq.seq)
        print('+')
        print(''.join(['I' for j in range(0, len(seq))]))