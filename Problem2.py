import pandas as pd
import itertools
from collections import Counter,defaultdict
import math
import scipy
from scipy.sparse import csr_matrix
from scipy.sparse.linalg import norm
import numpy as np
data=pd.read_csv("Processed_training.csv", header=None)
sentences=data[3]+data[4] #323257

L=3
dicts=[]
for sentence in sentences:
    v=str(sentence).split()
    for l in range(1,L+1):
        i=0
        while i <= len(v)-L-1:
            dicts.append([v[0+i],v[L+i]])
            i=i+1

def Compute_ppmi(dicts):
    #return dicts
    #number of times a word w appeared in dataset
    words=[item[0] for item in dicts]
    contexts=[item[1] for item in dicts]
    w_count=Counter(words)
    c_count=Counter(contexts)
    #number of distinct words in the dataset
    all_words=list(set(words+contexts))
    n=len(all_words) #99897
    unique_sets=list(dicts for dicts,_ in itertools.groupby(dicts)) #18780028
    D=len(unique_sets)

    #counting frequencies of pairs in distance L in document
    pair_counts=defaultdict(Counter)
    for k,v in dicts:
        pair_counts[k][v]+=1

    #For each unique_pair compute the ppmi.
    ppmi_v=[]
    unique_words_index=[]
    unique_c_index=[]
    for w,c in unique_sets:
        ppmi=round(math.log((pair_counts[w][c]*D)/float(w_count[w]*c_count[c])),4)
        ppmi_v.append(ppmi)
        print ppmi
        unique_words_index.append(all_words.index(w))
        unique_c_index.append(all_words.index(c))

    #Create the csr ppmi_matrix
    ppmi_matrix=csr_matrix((ppmi_v,(unique_words_index,unique_c_index)),shape=(n,n))

    #Save matrix to computer
    def save_sparse_csr(filename, array):
    # note that .npz extension is added automatically
        np.savez(filename, data=array.data, indices=array.indices,indptr=array.indptr, shape=array.shape)

    save_sparse_csr('ppmi_training', ppmi_matrix)

    #save unique lsts
    np.savetxt('word_ls',all_words)


    return ppmi_matrix

matrix=Compute_ppmi(sentences)
#What is the shape of the PPMI matrix?
print matrix.shape
#How many non-zero elements in this matrix?
print len(np.nonzero(matrix.toarray())[0])
#Frobenious norm of this matrix
fro_norm=norm(matrix, ord='fro')
print fro_norm
