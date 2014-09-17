#include<iostream>
#include<string>
using namespace std;
#define NumericIndictor 0x01
#define AlphaIndictor 0x02
#define NumericDec 3
#define NumericBin 10
#define NumericCounter 10
int FinderTab[49]= {3,3,3,3,3,3,3,
                    3,2,2,2,2,2,3,
                    3,2,3,3,3,2,3,
                    3,2,3,3,3,2,3,
                    3,2,3,3,3,2,3,
                    3,2,2,2,2,2,3,
                    3,3,3,3,3,3,3
                   };
int AlignTab[25]= {3,3,3,3,3,
                   3,2,2,2,3,
                   3,2,3,2,3,
                   3,2,2,2,3,
                   3,3,3,3,3
                  };
unsigned short NumericGettor[]= {0x200,0x100,
                                 0x080,0x040,0x020,0x010,
                                 0x008,0x004,0x002,0x001
                                };
unsigned char NumericGettor4[]= {0x8,0x4,0x2,0x1};
unsigned char NumericGettor7[]= {0x40,0x20,0x10,
                                 0x08,0x04,0x02,0x01
                                };
unsigned char alpha_to[256];
int index_of[256];
unsigned char alpha_to4[16];
int index_of4[16];
unsigned char BitGettor[]= {0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x01};
int OddPadingGettor[]= {1,1,1,0,1,1,0,0};
int EvenPadingGettor[]= {0,0,0,1,0,0,0,1};
struct ModeStruct
{
    unsigned char modeIndictor;
    unsigned short chCounter;
    int binCode[1024];
};
void encodeNumeric(char *num,int *result,int &reslen)
{
    ModeStruct numMod;
    char *reserve=num;
    numMod.modeIndictor=NumericIndictor;
    int len=strlen(num);
    numMod.chCounter=len;
    int fullCh=len/NumericDec;
    int idx,binIdx,inner;
    binIdx=0;
    unsigned short code;
    for(idx=0; idx<fullCh; idx++)
    {
        code=0;
        code+=(*num-'0')*100;
        num++;
        code+=(*num-'0')*10;
        num++;
        code+=*num-'0';
        num++;
        for(inner=0; inner<NumericBin; inner++)
            numMod.binCode[binIdx++]=((code&NumericGettor[inner])?1:0);

    }
    code=0;
    switch(len%NumericDec)
    {
    case 1:
        code+=*num-'0';
        for(idx=0; idx<4; idx++)
            numMod.binCode[binIdx++]=((code&NumericGettor4[idx])?1:0);
        break;
    case 2:
        code+=(*num-'0')*10;
        num++;
        code+=*num-'0';
        for(idx=0; idx<7; idx++)
            numMod.binCode[binIdx++]=(code&NumericGettor7[idx])?1:0;
        break;
    default:
        break;
    }
    reslen=(4+NumericCounter+binIdx);
    int lend=8-reslen%8;
    reslen+=lend;

    for(idx=0; idx<4; idx++)
        result[idx]=(numMod.modeIndictor&NumericGettor4[idx])?1:0;
    for(idx=4; idx<4+NumericCounter; idx++)
        result[idx]=(numMod.chCounter&NumericGettor[idx-4])?1:0;
    for(idx=NumericCounter+4; idx<reslen-lend; idx++)
        result[idx]=numMod.binCode[idx-4-NumericCounter];
    for(idx=reslen-lend; idx<reslen; idx++)
        result[idx]=0;


//    cout<<reserve<<endl;
//    for(idx=0; idx<4; idx++)
//        cout<<((numMod.modeIndictor&NumericGettor4[idx])?1:0);
//    cout<<" ";
//    for(idx=0; idx<NumericCounter; idx++)
//        cout<<((numMod.chCounter&NumericGettor[idx])?1:0);
//    cout<<" ";
//    for(idx=0; idx<binIdx; idx++)
//    {
//        if(idx%10==0)
//            cout<<" ";
//        cout<<numMod.binCode[idx];
//
//    }
//    cout<<endl;



}
void GenerateGF()
{
    int idx;
    alpha_to[0]=0x01;
    index_of[1]=0;
    for(idx=1; idx<255; idx++)
    {
        if(alpha_to[idx-1]&0x80)
            alpha_to[idx]=(alpha_to[idx-1]<<1)^0x11d;
        else
            alpha_to[idx]=alpha_to[idx-1]<<1;
        index_of[alpha_to[idx]]=idx;
    }
    index_of[0]=-1;
    //GF(2^4)
    alpha_to4[0]=0x01;
    index_of4[1]=0;
    for(idx=1; idx<15; idx++)
    {
        if(alpha_to4[idx-1]&0x8)
            alpha_to4[idx]=((alpha_to4[idx-1]<<1)^19);
        else
            alpha_to4[idx]=alpha_to4[idx-1]<<1;
        index_of4[alpha_to4[idx]]=idx;
    }
    index_of4[0]=-1;
//    for(idx=0; idx<255; idx++)
//        cout<<dec<<idx<<"\t"<<hex<<(int)alpha_to[idx]<<endl;

}
void padCodeword(int *code,int orilen,int extlen)
{
    int wordlen=extlen-orilen;
    int idx,*fetch,inner;
    for(idx=0; idx<wordlen/8; idx++)
    {
        fetch=code+orilen+idx*8;
        if(idx%2==0)
            for(inner=0; inner<8; inner++)
                fetch[inner]=OddPadingGettor[inner];
        else
            for(inner=0; inner<8; inner++)
                fetch[inner]=EvenPadingGettor[inner];
    }
}
void Bin2Word(unsigned char *m,int *code,int codelen)
{
    int grouplen=codelen/8;
    int idx,*codeSeek,inner;
    for(idx=0; idx<grouplen; idx++)
    {
        codeSeek=code+idx*8;
        for(inner=0; inner<8; inner++)
            if(codeSeek[inner])
                m[idx]|=BitGettor[inner];
            else
                m[idx]&=~BitGettor[inner];
    }
}
void fillMatrix()
{
    int a[8][9]= {0};
    int idx,inner;
    for(idx=3; idx<5; idx++)
        for(inner=6; inner<9; inner++)
            a[idx][inner]=117;
    int fillNum=0;
    for(idx=8; idx>=0; idx-=2)
    {
        for(inner=7; inner>=0; inner--)
        {
            if(!a[inner][idx])
                a[inner][idx]=fillNum++;
            if(idx>0)
                if(!a[inner][idx-1])
                    a[inner][idx-1]=fillNum++;
        }
    }
    for(idx=0; idx<8; idx++)
    {
        for(inner=0; inner<9; inner++)
            cout<<dec<<a[idx][inner]<<"\t";
        cout<<endl;
    }


}
void Encode10_26(unsigned char* m)
{
    int idx,inner,outer,k=16;
    int n=26;
    unsigned char r[n-k],tmp,;



    int g[11]= {45,32,94,64,70,118,61,46,67,251,0};

    for(idx=0; idx<n-k; idx++)
        r[idx]=0;

    for(idx=k-1; idx>=0; idx--)
    {
        tmp=index_of[r[n-k-1]^m[k-idx-1]];
        for(inner=n-k-1; inner>0; inner--)
            r[inner]=r[inner-1]^alpha_to[(g[inner]+tmp)%255];
        r[0]=alpha_to[(g[0]+tmp)%255];
    }
    for(idx=k; idx<n; idx++)
        m[idx]=r[n-1-idx];
    cout<<"*****************************"<<endl;
    for(idx=0; idx<n-k; idx++)
        cout<<hex<<"0x:"<<(int)r[idx]<<"  ";
    cout<<endl;
    fillMatrix();

}
void fillBlock()
{
    int mtx[4][8];
    int blocksize[]= {4,7,8,8};
    int data=0;
    int rowIdx=0;
    int colIdx=0;
    int rpt,clp;
    int idx,inner;
    int rowEff=4;
    int total=0;
    for(idx=0; idx<4; idx++)
        total+=blocksize[idx];
    int rowUpdate=0;
    for(idx=0; idx<blocksize[3]*4; idx++)
    {
        //cout<<"rowIdx"<<rowIdx<<endl;

        if(colIdx>=blocksize[rowIdx])
            rowIdx++;
        else
        {
            mtx[rowIdx][colIdx]=data++;
            cout<<"("<<rowIdx<<","<<colIdx<<")"<<endl;
            rowIdx++;

        }
        if(rowIdx==4)
        {
            rowIdx=0;
            colIdx++;
        }

    }




    for(rpt=0; rpt<4; rpt++)
    {
        for(clp=0; clp<blocksize[rpt]; clp++)
            cout<<dec<<mtx[rpt][clp]<<"\t";
        cout<<endl;
    }

}
void RSPoly()
{
    int g[20][20];
    g[1][0]=0;
    g[1][1]=0;


    int idx,inner;
    for(idx=2; idx<20; idx++)
    {
        g[idx][0]=(g[idx-1][0]+idx-1)%255;
        g[idx][idx]=0;
        for(inner=1; inner<idx; inner++)
            g[idx][inner]=index_of[alpha_to[g[idx-1][inner-1]]^alpha_to[(g[idx-1][inner]+idx-1)%255]];

    }
    cout<<"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"<<endl;
    for(idx=2; idx<20; idx++)
    {
        cout<<idx<<"\t";
        for(inner=idx; inner>=0; inner--)
            cout<<g[idx][inner]<<" ";
        cout<<endl;
    }


}
int ch2binLen(int type,int chlen)
{
    if(type==1)
    {
        int r=chlen%3;
        int q=chlen/3;
        q*=10;
        if(r!=0)
            r=10*r/3+1;
        return 14+q+r;
    }
    else if(type==2)
    {
        int r=chlen%2;
        int q=chlen/2;
        q=q*11;
        if(r!=0)
            r=11*r/2+1;
        return 13+q+r;
    }
    else
    {
        return 12+chlen*8;

    }
}
void compareBitLen(int pre,int cur,int nxt,int len)
{
    int cchg;
    int cbitg;
    int ccntlen;
    int nchg=1;
    int nbitg=8;
    int ncntlen=8;
    if(pre==nxt)
    {
        if(cur==1)
        {
            cchg=3;
            cbitg=10;
            ccntlen=10;
        }
        else if(cur==2)
        {
            cchg=2;
            cbitg=11;
            ccntlen=9;
        }
        //cout<<len<<" "<<cbitg<<" "<<cchg<<" "<<endl;
        //int result=4+ccntlen+4+ncntlen+((len/cchg)*cbitg);
        int result=4+ccntlen+((len/cchg)*cbitg);

        if(len%cchg)
        {
            //cout<<"abc"<<endl;
            result+=(((len%cchg)*cbitg/cchg)+1);
        }
        int nochange=(len/nchg)*nbitg;
        if(len%nchg)
            nochange+=((len%nchg)*nbitg/nchg+1);
        cout<<len<<" "<<nochange<<" "<<result<<endl;

    }
}
void printChLen()
{
    int idx;
    cout<<"*********************************"<<endl;
    for(idx=1; idx<11; idx++)
        cout<<idx<<" "<<ch2binLen(1,idx)<<" "<<ch2binLen(2,idx)<<" "<<ch2binLen(4,idx)<<endl;
    cout<<"no switch"<<"               "<<"switch"<<endl;
    for(idx=1; idx<16; idx++)
        compareBitLen(4,2,4,idx);
}
void PrintArrayPointer()
{
    int a[][3]= {1,2,3,6,7,8,2,5,9};
    int (*p)[3]=a;
    for(int idx=0; idx<3; idx++)
    {
        for(int inner=0; inner<3; inner++)
            cout<<p[idx][inner]<<"\t";
        cout<<endl;
    }
}
bool isNumeric(char num)
{
    return num>='0'&&num<='9';
}
bool isAlphanumeric(char alpha)
{
    if(alpha>='A'&&alpha<='Z')
        return true;
    else
    {
        switch(alpha)
        {
        case ' ':
        case '$':
        case '%':
        case '*':
        case '+':
        case '-':
        case '.':
        case '/':
        case ':':
            return true;
        default:
            return false;
        }
    }
}
int getMod(char c)
{
    if(isNumeric(c))
        return 1;
    else if(isAlphanumeric(c))
        return 2;
    else
        return 4;
}
void classifyCharacter()
{
    int idx;
    char *str="123qweFD@@#@$$#$#$S343";
    int len=strlen(str);
    char*p=str;
    cout<<strlen(str)<<endl;
    int preMod;
    int modCount=0;
    int nxtMod;
    int Record[50];
    Record[modCount*2]=getMod(*p++);
    Record[modCount*2+1]=1;
    for(idx=1; idx<len; idx++)
    {
        nxtMod=getMod(*p++);
        if(Record[modCount*2]!=nxtMod)
        {
            modCount++;
            Record[modCount*2]=nxtMod;
            Record[modCount*2+1]=1;
        }
        else
        {
            Record[modCount*2+1]++;
        }
    }
    cout<<str<<endl;
    for(idx=0; idx<=modCount; idx++)
        cout<<Record[idx*2]<<" "<<Record[idx*2+1]<<"\t";
}
void swapab(int &a,int &b)
{
    int tmp=a;
    a=b;
    b=tmp;
}
void testReference()
{
    int c=2;
    int d=8;
    int *a=&c;
    int *b=&d;
    cout<<*a<<"   "<<*b<<endl;
    swapab(*a,*b);
    cout<<*a<<"   "<<*b<<endl;
}
void testDecodeBCH()
{
    int r[50]= {0};
    int rlen;
    int S[10]= {0};
    int Slen;
    int sigma[10][10]= {-1};
    int mu[10]= {0};
    int nu[15];
    int e[15];

    int d[10]= {0};
    int l[10]= {0};
    int mu_l[10]= {0};
    int idx,inner;
    cout<<"***************testDecodeBCH**************************"<<endl;
    for(idx=0; idx<10; idx++)
        for(inner=0; inner<10; inner++)
            sigma[idx][inner]=-1;
    r[3]=r[6]=r[9]=1;
    rlen=15;
    Slen=6;
    for(idx=0; idx<16; idx++)
        cout<<(int)alpha_to4[idx]<<" "<<index_of4[idx]<<endl;
    unsigned char ctmp;
    for(idx=0; idx<Slen; idx++)
    {
        ctmp=0;
        for(inner=0; inner<rlen; inner++)
            if(r[inner]!=0)
                ctmp^=alpha_to4[((idx+1)*inner)%15];
        S[idx]=index_of4[ctmp];
    }
    cout<<"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"<<endl;
    for(idx=0; idx<10; idx++)
        cout<<S[idx]<<"   ";
    cout<<endl;
    mu[0]=-1;
    sigma[0][0]=0;
    d[0]=0;
    l[0]=0;
    mu_l[0]=-1;
    mu[1]=0;
    sigma[1][0]=0;
    d[1]=S[0];
    l[1]=0;
    mu_l[1]=0;
    int delta[10];
    int rho;
    int tau;
    int epsilon;
    unsigned char dalpha;
    int mdtmp;
    for(idx=2; idx<Slen+2; idx++)
    {
        mu[idx]=idx-1;
//        cout<<"abcE"<<endl;
        if(d[idx-1]==-1)
        {
            cout<<"abcF"<<endl;

            for(inner=0; inner<=l[idx-1]; inner++)
                sigma[idx][inner]=sigma[idx-1][inner];
            l[idx]=l[idx-1];
            cout<<"abcF"<<endl;
        }
        else
        {
            rho=idx-2;
            for(inner=0; inner<10; inner++)
                delta[inner]=-1;
            cout<<"abcG"<<endl;
            while(1)
            {
                if(d[rho]!=-1)
                    break;
                else
                    rho--;
            }
            mdtmp=d[idx-1]-d[rho];
            while(mdtmp<0)
            {
                mdtmp+=15;
            }
            tau=mdtmp%15;
            cout<<"tau "<<tau<<endl;
            epsilon=mu[idx-1]-mu[rho];
            cout<<"mu "<<mu[idx-1]<< "  "<<mu[rho]<<"   "<<rho<<endl;
            for(inner=l[rho]; inner>=0; inner--)
                delta[inner+epsilon]=sigma[rho][inner];

            for(inner=0; inner<=l[rho]+mu[idx-1]-mu[rho]; inner++)
            {
                if(delta[inner]!=-1)
                {
                    mdtmp=delta[inner]+tau;
                    while(mdtmp<0)
                    {
                        mdtmp+=15;
                    }
                    delta[inner]=mdtmp%15;
                }

                cout<<"delta "<<delta[inner]<<"   ";
            }
            cout<<endl;
            l[idx]=(l[idx-1]>l[rho]+mu[idx-1]-mu[rho])?l[idx-1]:l[rho]+mu[idx-1]-mu[rho];
            cout<<"lidx "<<l[idx]<<"  "<<sigma[1][1]<<endl;
            for(inner=0; inner<=l[idx]; inner++)
                if(sigma[idx-1][inner]==-1||delta[inner]==-1)
                {
                    if(sigma[idx-1][inner]==-1&&delta[inner]!=-1)
                        sigma[idx][inner]=delta[inner];
                    else if(sigma[idx-1][inner]!=-1&&delta[inner]==-1)
                        sigma[idx][inner]=sigma[idx-1][inner];
                    else
                        sigma[idx][inner]=-1;
                }
                else
                {
                    sigma[idx][inner]=index_of4[(alpha_to4[sigma[idx-1][inner]]^alpha_to4[delta[inner]])];
                    cout<<"EEELLLL "<<inner<<endl;
                }


        }
        if(idx==Slen+1)
            break;
//        cout<<"abcA"<<endl;
        mu_l[idx]=mu[idx]-l[idx];

        cout<<"l[idx]   "<<l[idx]<<endl;
//        cout<<"abcB"<<endl;
        dalpha=0;
//        cout<<"abcC"<<endl;
        for(inner=0; inner<=l[idx]; inner++)
            if(sigma[idx][inner]!=-1)
            {
                mdtmp=sigma[idx][inner]+S[mu[idx]-inner];
                while(mdtmp<0)
                {
                    mdtmp+=15;
                }
                dalpha^=(alpha_to4[mdtmp%15]);

            }
//        cout<<"abcD"<<endl;
        d[idx]=index_of4[dalpha];

    }


    cout<<endl;
    cout<<"**********************sigma******************"<<endl;
    for(idx=0; idx<=l[Slen+1]; idx++)
        cout<<sigma[Slen+1][idx]<<"   ";
    for(idx=0; idx<rlen; idx++)
        e[idx]=0;
    for(idx=0; idx<15; idx++)
    {
        ctmp=0;
        for(inner=0; inner<=l[Slen]; inner++)
            if(sigma[Slen+1][inner]!=-1)
            {
                mdtmp=idx*inner+sigma[Slen+1][inner];
                while(mdtmp<0)
                    mdtmp+=15;
                    //cout<<"@@@   "<<idx<<"   "<<inner<<"   "<<mdtmp%15<<endl;
                ctmp^=alpha_to4[mdtmp%15];

            }
        if(ctmp==0)
            e[15-idx]=1;
    }
    for(idx=0; idx<rlen; idx++)
        nu[idx]=(e[idx]==r[idx])?0:1;
    cout<<endl;
    cout<<"*************************RECEIVE****************************"<<endl;
    for(idx=0; idx<rlen; idx++)
        cout<<r[idx]<<"   ";
    cout<<endl;
    cout<<"*********************EEEEEEEEEEEEEEE***********************"<<endl;
    for(idx=0; idx<rlen; idx++)
        cout<<e[idx]<<"   ";
    cout<<endl;
    cout<<"***********************MODIFY*****************************"<<endl;

    for(idx=0; idx<rlen; idx++)
        cout<<nu[idx]<<"   ";
    cout<<"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"<<endl;
    for(idx=2; idx<Slen+2; idx++)
    {
        for(inner=0; inner<=l[idx]; inner++)

            cout<<sigma[idx][inner]<<" ";
        cout<<endl;

    }
    cout<<"############"<<endl;
    int qwe=-6;
    while(qwe<0)
    {
        qwe+=15;
    }
    cout<<qwe%15<<endl;





}
