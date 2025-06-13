import pandas as pd
import numpy as np
from dbfread import DBF

pf = '../../../data/01-judicial/00-sentencing/raw/Judiciales_BD_Catalogos_2009_dbf/'

table = DBF(f'{pf}CatalogosMicrodatos_2009/CDEL2009.DBF')


df = pd.DataFrame(iter(table))
df.to_excel(f'{pf}CatalogosMicrodatos_2009/CDEL2009.xlsx', index = False)


### 1997-2000
pf1 = '../../../data/01-judicial/00-sentencing/raw/judiciales_bd_catalogos_2000_dbf/judiciales_bd_catalogos_2000/CatalogosMicrodatos_2000'
table = DBF(f'{pf1}/cdel2000.DBF')

df = pd.DataFrame(iter(table))
df.to_excel(f'{pf1}/cdel2000.xlsx', index = False)