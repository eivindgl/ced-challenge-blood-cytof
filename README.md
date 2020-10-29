# cytof-challenge-new-panel

A mixed bag of sketches and final version of code for a cytof CeD challenge paper autum 2020.
Needs some cleanup before making this public. The current purpose is as a reference for a later date.
E.g. questions, code sharing etc.

Cytof data and sample info is left out of this repository.
Note, one sample had the wrong metadata set (tetneg instead of tetpos, if I recall correctly).
However, the file name indicates the correct sample type. Check this if the old metadata file is used at a later date.
The structure of sample data and metadata files left out of this repo is as follows:

```
data
└── 2008265_AC
    ├── 200825 Beskrivelse av analysen.docx
    ├── CD4_BL_EN_TetPos_neg
    │   ├── 1299_EN_BL_TetNeg.fcs
    │   ├── 1299_EN_BL_TetPos.fcs
    │   ├── 1568_EN_BL_TetNeg.fcs
    │   ├── 1568_EN_BL_TetPos.fcs
    │   ├── 1577_EN_BL_TetNeg.fcs
    │   ├── 1577_EN_BL_TetPos.fcs
    │   ├── 1582_EN_BL_TetNeg.fcs
    │   ├── 1582_EN_BL_TetPos.fcs
    │   ├── 2191_EN_BL_TetNeg.fcs
    │   ├── 2191_EN_BL_TetPos.fcs
    │   ├── 2192_EN_BL_TetNeg.fcs
    │   ├── 2192_EN_BL_TetPos.fcs
    │   ├── 2193_EN_BL_TetNeg.fcs
    │   ├── 2193_EN_BL_TetPos.fcs
    │   ├── 2194_EN_BL_TetNeg.fcs
    │   ├── 2194_EN_BL_TetPos.fcs
    │   ├── 2206_EN_BL_TetNeg.fcs
    │   ├── 2206_EN_BL_TetPos.fcs
    │   ├── 431_EN_BL_TetNeg.fcs
    │   └── 431_EN_BL_TetPos.fcs
    ├── CD4_BL_EN_TetPos_Tem_b7pos
    │   ├── 1299EN_BL_TetPos_Tem_b7Pos.fcs
    │   ├── 1568EN_BL_TetPos_Tem_b7Pos.fcs
    │   ├── 1577EN_BL_TetPos_Tem_b7Pos.fcs
    │   ├── 1582EN_BL_TetPos_Tem_b7Pos.fcs
    │   ├── 2191EN_BL_TetPos_Tem_b7Pos.fcs
    │   ├── 2192EN_BL_TetPos_Tem_b7Pos.fcs
    │   ├── 2193EN_BL_TetPos_Tem_b7Pos.fcs
    │   ├── 2194EN_BL_TetPos_Tem_b7Pos.fcs
    │   ├── 2206EN_BL_TetPos_Tem_b7Pos.fcs
    │   └── 431EN_BL_TetPos_Tem_b7Pos.fcs
    ├── CD4_d6_EN_TetPos_Neg
    │   ├── 1299_EN_d6_TetNeg.fcs
    │   ├── 1299_EN_d6_TetPos.fcs
    │   ├── 1568_EN_d6_TetNeg.fcs
    │   ├── 1568_EN_d6_TetPos.fcs
    │   ├── 1577_EN_d6_TetNeg.fcs
    │   ├── 1577_EN_d6_TetPos.fcs
    │   ├── 1582_EN_d6_TetNeg.fcs
    │   ├── 1582_EN_d6_TetPos.fcs
    │   ├── 2191_EN_d6_TetNeg.fcs
    │   ├── 2191_EN_d6_TetPos.fcs
    │   ├── 2192_EN_d6_TetNeg.fcs
    │   ├── 2192_EN_d6_TetPos.fcs
    │   ├── 2193_EN_d6_TetNeg.fcs
    │   ├── 2193_EN_d6_TetPos.fcs
    │   ├── 2194_EN_d6_TetNeg.fcs
    │   ├── 2194_EN_d6_TetPos.fcs
    │   ├── 2206_EN_d6_TetNeg.fcs
    │   ├── 2206_EN_d6_TetPos.fcs
    │   ├── 431_EN_d6_TetNeg.fcs
    │   └── 431_EN_d6_TetPos.fcs
    ├── CD4_d6_EN_TetPos_Tem_b7pos
    │   ├── 1299EN_d6_TetPos_Tem_b7Pos.fcs
    │   ├── 1568EN_d6_TetPos_Tem_b7Pos.fcs
    │   ├── 1577EN_d6_TetPos_Tem_b7Pos.fcs
    │   ├── 1582EN_d6_TetPos_Tem_b7Pos.fcs
    │   ├── 2191EN_d6_TetPos_Tem_b7Pos.fcs
    │   ├── 2192EN_d6_TetPos_Tem_b7Pos.fcs
    │   ├── 2193EN_d6_TetPos_Tem_b7Pos.fcs
    │   ├── 2194EN_d6_TetPos_Tem_b7Pos.fcs
    │   ├── 2206EN_d6_TetPos_Tem_b7Pos.fcs
    │   └── 431EN_d6_TetPos_Tem_b7Pos.fcs
    ├── CD4_PRE_BL_d6
    │   ├── 1299_PRE_CD4_BL.fcs
    │   ├── 1299_PRE_CD4_d6.fcs
    │   ├── 1568_PRE_CD4_BL.fcs
    │   ├── 1568_PRE_CD4_d6.fcs
    │   ├── 1577_PRE_CD4_BL.fcs
    │   ├── 1577_PRE_CD4_d6.fcs
    │   ├── 1582_PRE_CD4_BL.fcs
    │   ├── 1582_PRE_CD4_d6.fcs
    │   ├── 2191_PRE_CD4_BL.fcs
    │   ├── 2191_PRE_CD4_d6.fcs
    │   ├── 2192_PRE_CD4_BL.fcs
    │   ├── 2192_PRE_CD4_d6.fcs
    │   ├── 2193_PRE_CD4_BL.fcs
    │   ├── 2193_PRE_CD4_d6.fcs
    │   ├── 2194_PRE_CD4_BL.fcs
    │   ├── 2194_PRE_CD4_d6.fcs
    │   ├── 2195_PRE_CD4_BL.fcs
    │   ├── 2195_PRE_CD4_d6.fcs
    │   ├── 2206_PRE_CD4_BL.fcs
    │   ├── 2206_PRE_CD4_d6.fcs
    │   ├── 431_PRE_CD4_BL.fcs
    │   └── 431_PRE_CD4_d6.fcs
    ├── CD4_PRE_Tem_b7pos_BL_d6
    │   ├── 1299PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 1299PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 1568PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 1568PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 1577PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 1577PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 1582PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 1582PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 2191PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 2191PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 2192PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 2192PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 2193PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 2193PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 2194PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 2194PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 2206PRE_BL_CD4_Tem_b7Pos.fcs
    │   ├── 2206PRE_d6_CD4_Tem_b7Pos.fcs
    │   ├── 431PRE_BL_CD4_Tem_b7Pos.fcs
    │   └── 431PRE_d6_CD4_Tem_b7Pos.fcs
    ├── Fil- og markøroversikt
    │   ├── 200825 files for prediction.xlsx
    │   ├── 200825 files for ranking.xlsx
    │   └── 200825 markers for ranking.xlsx
    └── UCeD
        ├── UCeD_Pre_AllCells
        │   ├── UCeD2048_CD4.fcs
        │   ├── UCeD5023_CD4.fcs
        │   ├── UCeD5052_CD4.fcs
        │   └── UCeD5053_CD4.fcs
        ├── UCeD_PRE_Tem_b7Pos
        │   ├── UCeD2048_CD4_Tem_b7Pos.fcs
        │   ├── UCeD5023_CD4_Tem_b7Pos.fcs
        │   ├── UCeD5052_CD4_Tem_b7Pos.fcs
        │   └── UCeD5053_CD4_Tem_b7Pos.fcs
        ├── UCeD_TetNeg_AllCells
        │   ├── UCeD5023_TetNeg.fcs
        │   ├── UCeD5048_TetNeg.fcs
        │   ├── UCeD5052_TetNeg.fcs
        │   └── UCeD5053_TetNeg.fcs
        └── UCeD_TetPos_Tem_b7Pos
            ├── UCeD2048_TetPos_Tem_b7Pos.fcs
            ├── UCeD5023_TetPos_Tem_b7Pos.fcs
            ├── UCeD5052_TetPos_Tem_b7Pos.fcs
            └── UCeD5053_TetPos_Tem_b7Pos.fcs

```
