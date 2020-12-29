# Outputs full fasta
def Readfile(fileName):
    #Create dictionary
    content=[]
    with open(fileName) as f:
        while True:
            #strip newline
            line=f.readline().rstrip("\n")
            if line.startswith(">"):
                id=line.replace(">seq.","") #number is left
                fragment=f.readline().rstrip("\n")
                #Add into directory
                content.append((id,fragment))
            elif not line:
                break
    f.close()
    return content

def Assemble_genome(left,right,overlap):

    """Input is tuple with fragment id and fragment
        output is the string of both left-right united"""
    left_id=int(left[0])
    right_id=int(right[0])
    left=left[1]
    right=right[1]
    overlap=overlap

    if right_id > left_id+3:
        #Problem comes when there between seq 1-5 it adds 1 more char at pos 51 similar to pos 50


    #computes overlap between two fragments
    if left.endswith('N'):
        #This is made for the two strings to link together
        right='N'+right

    for i in range(len(left)):
        if left[i:] == right[:len(left)-i]:
            #there is an overlap here: left[i:]
            return left[0:i-1]+left[i:]+right[len(left)-i:]
    #if overlap doesnt exist add N's
    return 'N'*overlap



def get_genome(reads):

    genome=reads[0]
    for i in range(1,len(reads)):
        genome=(reads[i-1],Assemble_genome(genome,reads[i],50))
    return genome

#IMPORTS
import re
import sys
import os


if __name__== "__main__":
    header_file=sys.argv[1] # input the header_file
    header_file=header_file.rstrip("\n")
    wd='/usr/mic/post1/metagenomics/ref_sets/fasta/01012015update/sinceMar2014'#depending the header file we do the wd
    folder_dir=header_file.replace("headers","dir")#name of the directory to look out for the sequences
    with open(header_file) as hf:
        #change folder
        os.chdir(folder_dir)
        g=open(folder+"+.giant_fasta.txt","a")
        for header in hf:
        #get seqid using re
            match=re.search('\[sequence_id (\d+)\]',header)
            seqid=match.group(1)
            os.chdir(seqid)
            if seqid+"_v2.fa.frag" in os.listdir("."):
                reads = readDataFromFile(seqid+"_v2.fa.frag")
                genome=get_genome(reads)
            else:
                #Nothing was taken from fasta file so just take original fasta
                genome=readDataFromFile(seqid+".fa")
        #open a new txt file giant_fasta and append header and genome information in
        #g=open(os.path.join(wd,folder_dir,folder+"+.giant_fasta.txt"))
            g.write(header)
            g.write(str(genome))
            os.chdir('..')
    hf.close()
    g.close()
