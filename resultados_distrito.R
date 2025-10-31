## Candidaturas por distrito
cands.2024 <- read.csv("CANDIDATURAS_2024.csv")

## Candidatura comun PVEM_PT_MORENA
cands.2024$pvem.pt.morena <- ifelse(cands.2024$PARTIDO_CI == "PVEM_PT_MORENA", 1, 0)

## Candidatura comun PAN_PRI_PRD
cands.2024$pan.pri.prd <- ifelse(cands.2024$PARTIDO_CI == "PAN_PRI_PRD", 1, 0)

## Identificador: estado.distrito
cands.2024$edo_dto <- paste(cands.2024$ID_ENTIDAD, sep = ".", cands.2024$ID_DISTRITO_FEDERAL)

## Alianza PVEM_PT_MORENA por distrito (indicadora)
alianzas.morena.2024 <- aggregate(x = c(cands.2024$pvem.pt.morena),
                                  by = list(cands.2024$edo_dto),
                                  FUN = sum)
names(alianzas.morena.2024)[names(alianzas.morena.2024) == "Group.1"] <- "edo_dto"   
names(alianzas.morena.2024)[names(alianzas.morena.2024) == "x"] <- "alianza.morena"


## Alianza PAN_PRI_PRD por distrito (indicadora)
alianzas.opos.2024 <- aggregate(x = c(cands.2024$pan.pri.prd),
                                by = list(cands.2024$edo_dto),
                                FUN = sum)
names(alianzas.opos.2024)[names(alianzas.opos.2024) == "Group.1"] <- "edo_dto"   
names(alianzas.opos.2024)[names(alianzas.opos.2024) == "x"] <- "alianza.oposic"

## Alianza PVEM_PT_MORENA y PAN_PRI_PRD por distrito (indicadora)
Alianzas.2024 <- merge(alianzas.morena.2024, alianzas.opos.2024, by = "edo_dto")

## Resultados por sección
dip.2024 <- read.csv("DIP_FED_2024.csv")

dip.2024$ENTIDAD <- as.factor(dip.2024$ENTIDAD)
dip.2024$DISTRITO_FEDERAL <- as.factor(dip.2024$DISTRITO_FEDERAL)

## Selección de variables a utilizar
vars <- names(dip.2024)
base2024 <- subset(dip.2024, select = vars[c(3:6, 13:32)])

## Identificador: estado.distrito
base2024$edo_dto <- paste(base2024$ID_ENTIDAD, sep = ".", base2024$ID_DISTRITO_FEDERAL)
base2024$edo_dto <- as.factor(base2024$edo_dto)

## Añadir columnas con alianzas
base2024 <- merge(base2024, Alianzas.2024, by = "edo_dto")


## Votos de coalición SHH
base2024$alianza.morena.votos <- ifelse(base2024$alianza.morena == 1,
                                        with(base2024, MORENA + PVEM_PT_MORENA + PVEM_PT +
                                               PVEM_MORENA + PT_MORENA + PT + PVEM), NA)

## Votos de Morena (donde no fue en alianza)
base2024$morena.solo <- ifelse(base2024$alianza.morena == 0, base2024$MORENA, NA)

## Votos de PT (donde no fue en alianza)
base2024$pt.solo <- ifelse(base2024$alianza.morena == 0, base2024$PT, NA)

## Votos de PVEM (donde no fue en alianza)
base2024$pvem.solo <- ifelse(base2024$alianza.morena == 0, base2024$PVEM, NA)

## Votos de coalición FCM
base2024$alianza.oposic.votos <- ifelse(base2024$alianza.oposic == 1,
                                        with(base2024, PAN + PRI + PRD + PAN_PRI_PRD + 
                                               PAN_PRI + PAN_PRD + PRI_PRD), NA)
## Votos de PAN (donde no fue en alianza)
base2024$pan.solo <- ifelse(base2024$alianza.oposic == 0, base2024$PAN, NA)

## Votos de PRI (donde no fue en alianza)
base2024$pri.solo <- ifelse(base2024$alianza.oposic == 0, base2024$PRI, NA)

## Votos de PRD (donde no fue en alianza)
base2024$prd.solo <- ifelse(base2024$alianza.oposic == 0, base2024$PRD, NA)

## Agregar resultados por secciones en resultados por distrito
alianza.morena <- aggregate(x = c(base2024$alianza.morena.votos),
                            by = list(base2024$edo_dto),
                            na.rm = TRUE, 
                            FUN = sum)
names(alianza.morena)[names(alianza.morena) == "Group.1"] <- "edo_dto"   
names(alianza.morena)[names(alianza.morena) == "x"] <- "alianza.morena"

morena.solo <- aggregate(x = c(base2024$morena.solo),
                         by = list(base2024$edo_dto),
                         na.rm = TRUE, 
                         FUN = sum)
names(morena.solo)[names(morena.solo) == "Group.1"] <- "edo_dto"   
names(morena.solo)[names(morena.solo) == "x"] <- "morena.solo"

pt.solo <- aggregate(x = c(base2024$pt.solo),
                     by = list(base2024$edo_dto),
                     na.rm = TRUE, 
                     FUN = sum)
names(pt.solo)[names(pt.solo) == "Group.1"] <- "edo_dto"   
names(pt.solo)[names(pt.solo) == "x"] <- "pt.solo"

pvem.solo <- aggregate(x = c(base2024$pvem.solo),
                       by = list(base2024$edo_dto),
                       na.rm = TRUE, 
                       FUN = sum)
names(pvem.solo)[names(pvem.solo) == "Group.1"] <- "edo_dto"   
names(pvem.solo)[names(pvem.solo) == "x"] <- "pvem.solo"

mc <- aggregate(x = c(base2024$MC),
                by = list(base2024$edo_dto),
                na.rm = TRUE, 
                FUN = sum)
names(mc)[names(mc) == "Group.1"] <- "edo_dto"   
names(mc)[names(mc) == "x"] <- "mc"

alianza.opo <- aggregate(x = c(base2024$alianza.oposic.votos),
                         by = list(base2024$edo_dto),
                         na.rm = TRUE, 
                         FUN = sum)
names(alianza.opo)[names(alianza.opo) == "Group.1"] <- "edo_dto"   
names(alianza.opo)[names(alianza.opo) == "x"] <- "alianza.opo"

pan.solo <- aggregate(x = c(base2024$pan.solo),
                      by = list(base2024$edo_dto),
                      na.rm = TRUE, 
                      FUN = sum)
names(pan.solo)[names(pan.solo) == "Group.1"] <- "edo_dto"   
names(pan.solo)[names(pan.solo) == "x"] <- "pan.solo"

pri.solo <- aggregate(x = c(base2024$pri.solo),
                      by = list(base2024$edo_dto),
                      na.rm = TRUE, 
                      FUN = sum)
names(pri.solo)[names(pri.solo) == "Group.1"] <- "edo_dto"   
names(pri.solo)[names(pri.solo) == "x"] <- "pri.solo"

prd.solo <- aggregate(x = c(base2024$prd.solo),
                      by = list(base2024$edo_dto),
                      na.rm = TRUE, 
                      FUN = sum)
names(prd.solo)[names(prd.solo) == "Group.1"] <- "edo_dto"   
names(prd.solo)[names(prd.solo) == "x"] <- "prd.solo"

ind <- aggregate(x = c(base2024$CANDIDATO.A.INDEPENDIENTE),
                 by = list(base2024$edo_dto),
                 na.rm = TRUE, 
                 FUN = sum)
names(ind)[names(ind) == "Group.1"] <- "edo_dto"   
names(ind)[names(ind) == "x"] <- "independientes"

resultados2024.distritos <- merge(alianza.morena, morena.solo, by = "edo_dto")
resultados2024.distritos <- merge(resultados2024.distritos, pt.solo, by = "edo_dto")
resultados2024.distritos <- merge(resultados2024.distritos, pvem.solo, by = "edo_dto")
resultados2024.distritos <- merge(resultados2024.distritos, alianza.opo, by = "edo_dto")
resultados2024.distritos <- merge(resultados2024.distritos, pan.solo, by = "edo_dto")
resultados2024.distritos <- merge(resultados2024.distritos, pri.solo, by = "edo_dto")
resultados2024.distritos <- merge(resultados2024.distritos, prd.solo, by = "edo_dto")
resultados2024.distritos <- merge(resultados2024.distritos, mc, by = "edo_dto")
resultados2024.distritos <- merge(resultados2024.distritos, ind, by = "edo_dto")

names(Alianzas.2024)[names(Alianzas.2024) == "alianza.morena"] <- "alianza.morena.si.no"
names(Alianzas.2024)[names(Alianzas.2024) == "alianza.opisic"] <- "alianza.opo.si.no"
resultados2024.distritos <- merge(resultados2024.distritos, Alianzas.2024, by = "edo_dto")

## total de votos válidos
resultados2024.distritos$validos <- with(resultados2024.distritos,
                                         alianza.morena + morena.solo + pt.solo +
                                           pvem.solo + alianza.opo + pan.solo + pri.solo +
                                           prd.solo + mc + independientes)

## Proporciones con respecto a total de votos válidos
resultados2024.distritos$alianza.morena.p <- with(resultados2024.distritos,
                                                  alianza.morena / validos)
resultados2024.distritos$morena.solo.p <- with(resultados2024.distritos,
                                               morena.solo / validos)
resultados2024.distritos$pt.solo.p <- with(resultados2024.distritos,
                                           pt.solo / validos)
resultados2024.distritos$pvem.solo.p <- with(resultados2024.distritos,
                                             pvem.solo / validos)
resultados2024.distritos$alianza.opo.p <- with(resultados2024.distritos,
                                               alianza.opo / validos)
resultados2024.distritos$pan.solo.p <- with(resultados2024.distritos,
                                            pan.solo / validos)
resultados2024.distritos$pri.solo.p <- with(resultados2024.distritos,
                                            pri.solo / validos)
resultados2024.distritos$prd.solo.p <- with(resultados2024.distritos,
                                            prd.solo / validos)
resultados2024.distritos$mc.p <- with(resultados2024.distritos,
                                      mc / validos)
resultados2024.distritos$independientes.p <- with(resultados2024.distritos,
                                                  independientes / validos)


## Distritos con dos coaliciones (sólo tres candidatos)
resultados2024.distritos$trescand <- with(resultados2024.distritos,
                                          ifelse(alianza.morena.si.no==1 & alianza.oposic == 1, 1, 0))

## Cálculo de índice HH
resultados2024.distritos$HH <- with(resultados2024.distritos,
                                    alianza.morena.p^2 + morena.solo.p^2 + pt.solo.p^2 +
                                      pvem.solo.p^2 + alianza.opo.p^2 + pan.solo.p^2 + 
                                      pri.solo.p^2 + prd.solo.p^2 + mc.p^2 + independientes.p^2)

## Cálculo de número efectivo de partidos
resultados2024.distritos$N <- 1 / resultados2024.distritos$HH

## Histograma y N promedio
library("ggstatsplot")
gghistostats(
  data = resultados2024.distritos,
  x = N,
  test.value = 2
)

## Descriptiva de N y HH
summary(resultados2024.distritos$N)
summary(resultados2024.distritos$HH)

