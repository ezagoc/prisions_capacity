import pandas as pd
import numpy as np
from dbfread import DBF

pf = '../../../data/01-judicial/00-sentencing/raw/Judiciales_BD_Catalogos_2009_dbf/'

table = DBF(f'{pf}CatalogosMicrodatos_2009/CDEL2009.DBF')


df = pd.DataFrame(iter(table))
df.to_excel(f'{pf}CatalogosMicrodatos_2009/CDEL2009.xlsx', index = False)