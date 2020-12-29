#Problem 3 is word2vec computation
import scipy.sparse
import numpy as np
import scipy
import pandas as pd

#####METHODS#####
def Compute_feature_v(s):
    words=s.split()
    q=set(words) # set of words in the sentence
    q_n=len(words) # number of words in sentence q
    sums=[sum(F[all_words.index(word),:]) for word in words]
    x_q=(1/q_n)*sums
    return x_q
#Compute feature vector for the sentence
def Compute_cosine(s1,s2):
#cosine similarity between q1,q2
    x1=Compute_feature_v(s1)
    x2=Compute_feature_v(s2)
    cos=np.transpose(x1).dot(x2)/np.norm(x1)*np.norm(x2)
    return cos

def Compute_Accuracy(q1_v, q2_v, lbl,thr):

    count=0
    for i in range(0, len(q1_v)):
        cos=Compute_cosine(q1_v[i],q2_v[i])
        semi_pred=sign(cos-thr)
        if semi_pred < 0:
            pred=0
        else:
            pred=1

        if str(lbl[i]) is str(pred):
            count+=1

    # Compute testing accuracy
    acc= round(count/float(n),4)
    print("For t=",t,"accuracy is:"acc,"\n")
    return acc

#INPUT THE CSR MATRIX AND all_words
loader=np.load("ppmi_training")
matrix=csr_matrix((loader['data'],loader['indices'], loader['indptr']),shape = loader['shape'])
all_words=pd.read_table('word_ls', header=None)

# Calculate the top-eigenvectors of matrix
#w=array of k eigenvalues, v = array rep the k eigenvectors v[:,i] wigv for eigval w[i]
w,v=scipy.sparse.linalg.eigsh(matrix, k=100)
F=w*v

data=pd.read_csv("Processed_training.csv", header=None)
q1_v=data[3]
q2_v=data[4]
labl=data[5]
thresholds=arange(0.8,1.2,0.2)
for t in thresholds:
    Compute_Accuracy(q1_v,q2_v,labl,t)

#Select best threshold and do it with the training dataset
data2=pd.read_csv("Processed_validation.csv", header=None)
q1_v=data2[3]
q2_v=data2[4]
labl=data2[5]
Compute_Accuracy(q1_v,q2_v,labl,t)
