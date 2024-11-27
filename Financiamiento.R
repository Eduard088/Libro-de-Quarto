financiamiento <- read_excel("C://Users//Eduardo//Documents//PC//Financiamiento.xlsx")

financiamiento <- financiamiento %>%
  gather(key=Categoria, value=Monto, Igualitario:Proporcional)

# Elimina filas con NA y valores de 0 en todas las columnas
financiamiento <- financiamiento %>%
  filter_all(all_vars(!is.na(.) & . != 0))

financiamiento <- financiamiento %>%
  select(-Concepto, Categoria)%>%
  group_by(Año, Partido) %>%
  reframe(Financiamiento = sum(Monto))


Elecciones <- read_excel("C:/Users/Eduardo/Documents/Tibble_mlar.xlsx")


financiamiento_MR <- financiamiento %>%
  filter(Partido=="MORENA")

Elecciones_MR <- Elecciones %>%
filter(Partido=="MORENA")

Congresosfed <- Congresosfed %>%
  inner_join(financiamiento, by=c("Año", "Partido"))

Senadofed <- senfed2018_2_f1 %>%
  inner_join(financiamiento_sen, by="Partido")

financiamiento_sen <- financiamiento %>%
  filter(Año==2018)


write.xlsx(Elecciones_1, "Elecciones_1.xlsx")



aa <-  financiamiento %>%
  filter(Concepto == "Actividades Específicas") %>%
  group_by(Año) %>%
  reframe(sum(Monto))

aa <- aa%>%
  rename(Monto = "sum(Monto)")

aa %>%
  reframe(sum(Monto))





aa %>%
  filter(Año==2021)%>%
  group_by(Año) %>%
  reframe(sum(Monto))


##
financiamientoind <- read_excel("C://Users//Eduardo//Documents//PC//FinanciamientoInd.xlsx") 

financiamientoind %>%
  group_by(Año) %>%
  reframe(sum(Monto))

