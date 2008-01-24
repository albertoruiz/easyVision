int getPoints32f(float * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2,
                 int max, int* tot, int* hp);

int lbp8u(int delta, unsigned char * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2, int* histogram);

int hsvcodeTest(int kb, int kg, int kw,
                unsigned char * pSrc, int sstep,
                int sr1, int sr2, int sc1, int sc2);
int hsvcode(int kb, int kg, int kw,
            unsigned char * pSrc, int sstep,
            unsigned char * pDst, int dstep,
            int sr1, int sr2, int sc1, int sc2);
