import csv
id_lookup={}
def loadLookup():
	with open("ugram.txt", 'rb') as csvfile:
		reader = csv.reader(csvfile, delimiter=' ', quotechar='\"')
		count=0
		for line in reader:
			key=line[1]
			val=line[0]
			id_lookup[key]=val
		print "error count",count


def indexify(filename,outfile,gram):
	out=open(outfile,'wb')
	csvwriter=csv.writer(out,delimiter=',')
	with open(filename, 'rb') as csvfile:
			reader = csv.reader(csvfile, delimiter=' ', quotechar='\"')
			count=0
			for line in reader:
				words=line[1].split()
				indexes=[]
				if(len(words)==gram):
					for i in range(gram):
						word=words[i]
						try:
							indexes.append(id_lookup[word])
						except:
							count=count+1
				indexes.append(line[2])
				csvwriter.writerow(indexes)

# Main

loadLookup()
print "bigram"
indexify('bgram.txt','bgram_index.csv',2)
print "trigram"
indexify('tgram.txt','tgram_index.csv',3)
print "qgram"
indexify('qgram.txt','qgram_index.csv',4)


#Tests
print "############Tests################"
print id_lookup["aaaaaaaaaahhhhhhhhhhhhhhhhhh"]