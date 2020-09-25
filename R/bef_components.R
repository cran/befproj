

#' Makes a local population projection and produce results for population
#' components
#'
#' @param startpop This is the start population
#' @param assumptions This is a Data Frame with assumptions
#' @param YEAR This is the year from which the forecast starts
#' @return The output from \code{\link{return}}
#' @export
#'
#' @examples bef_components(startpop_data,assump_data,2019)

bef_components<-function(startpop,assumptions,YEAR) {

  category <- NULL
  pop.components <- NULL
  pop.results <- NULL

  x<-assumptions[with(assumptions, order(category, age)),]
  x<-x[,2:length(x)]

  Startmatris<-startpop[with(startpop, order(age)),]
  Startmatris<-Startmatris[,2:length(Startmatris)]

  Startmatris_1<-as.matrix(Startmatris,col=2)
  colnames(Startmatris_1) <- c("N0_K","N0_M")

  Kol<-NCOL(assumptions)-1

  Fx<-dplyr::filter(x,category %in% c("asfr"))
  Fx<-as.matrix(Fx[,2:Kol])

  Inrikes_Inflyttandel_M<-dplyr::filter(x,category %in% c("inmig.rates.men"))
  Inrikes_Inflyttandel_M<-as.matrix(Inrikes_Inflyttandel_M[,2:Kol])
  Inrikes_Inflyttandel_K<-dplyr::filter(x,category %in% c("inmig.rates.women"))
  Inrikes_Inflyttandel_K<-as.matrix(Inrikes_Inflyttandel_K[,2:Kol])
  Lokal_Utflyttandel_M<-dplyr::filter(x,category %in% c("outmig.rates.men"))
  Lokal_Utflyttandel_M<-as.matrix(Lokal_Utflyttandel_M[,2:Kol])
  Lokal_Utflyttandel_K<-dplyr::filter(x,category %in% c("outmig.rates.women"))
  Lokal_Utflyttandel_K<-as.matrix(Lokal_Utflyttandel_K[,2:Kol])
  Riket_just_M<-dplyr::filter(x,category %in% c("natpop.men"))
  Riket_just_M<-as.matrix(Riket_just_M[,2:Kol])
  Riket_just_K<-dplyr::filter(x,category %in% c("natpop.women"))
  Riket_just_K<-as.matrix(Riket_just_K[,2:Kol])
  Netto_K<-dplyr::filter(x,category %in% c("intermig.net.women"))
  Netto_K<-as.matrix(Netto_K[,2:Kol])
  Netto_M<-dplyr::filter(x,category %in% c("intermig.net.men"))
  Netto_M<-as.matrix(Netto_M[,2:Kol])
  Px_M1<-dplyr::filter(x,category %in% c("asdr_men"))
  Px_M1<-as.matrix(Px_M1[,2:Kol])
  Px_K1<-dplyr::filter(x,category %in% c("asdr_women"))
  Px_K1<-as.matrix(Px_K1[,2:Kol])

  k <- c(-1)
  i<-0
  n<-NCOL(assumptions)-2


  my.list_M <- list()
  my.list_K <- list()
  my.list_TOT<- list()
  my.list_fodda_M<-list()
  my.list_fodda_K<-list()
  my.list_doda_K<-list()
  my.list_doda_M<-list()
  my.list_netto_M<-list()
  my.list_netto_K<-list()
  my.list_brutto_INRIKES_UTF_TOT<-list()
  my.list_brutto_INRIKES_INF_TOT<-list()
  my.list_MIG_NETTO_M<-list()
  my.list_MIG_NETTO_K<-list()
  utland.list_K<-list()
  utland.list_M<-list()
  livslangd_M<-list()

  N1_K<-Startmatris_1[,1]
  N1_M<-Startmatris_1[,2]


  N0_K<-N1_K
  N0_M<-N1_M


  funktion_medellivslangd <- function(x){
    Start<-100000
    n2<-length(Px_M2)
    b<-list()
    m1<-1
    for (m1 in 1:n2) {
      Start<-Start-(Start*Px_M2[m1])
      b[m1]<-Start
    }
    FIKTIV<-c(100000,unlist(b[1:n2-1]))
    AVLIDNA<-FIKTIV*Px_M2
    VEKTOR<-c(0.15,rep(0.5,n2-1))
    B2<-FIKTIV-AVLIDNA+AVLIDNA*VEKTOR
    MEDELLIVSLANGD<-rev(cumsum(rev(B2)))/FIKTIV
    return(MEDELLIVSLANGD)
  }



  for (i in 1:n) {
    k <- k+1
    i<-i+1

    Px_M2<-Px_M1[,k+1]
    Px_K2<-Px_K1[,k+1]
    Px_M3<-N1_M*Px_M2
    Px_K3<-N1_K*Px_K2
    N2a_M<-N1_M*(1-Px_M2)
    N2a_K<-N1_K*(1-Px_K2)
    Inrikes_Inflyttandel_M2<-c(Inrikes_Inflyttandel_M[,k+1])
    Inrikes_Inflyttandel_K2<-c(Inrikes_Inflyttandel_K[,k+1])
    Riket_just_M2<-c(Riket_just_M[,k+1])
    Riket_just_K2<-c(Riket_just_K[,k+1])
    Inflyttade_M<-c(Inrikes_Inflyttandel_M2*Riket_just_M2)
    Inflyttade_K<-c(Inrikes_Inflyttandel_K2*Riket_just_K2)
    Lokal_Utflyttandel_M2<-c(Lokal_Utflyttandel_M[,k+1])
    Lokal_Utflyttandel_K2<-c(Lokal_Utflyttandel_K[,k+1])
    Utflyttade_M<-Lokal_Utflyttandel_M2*N2a_M
    Utflyttade_K<-Lokal_Utflyttandel_K2*N2a_K

    NETTO_MIG_M2<-c(Netto_M[,k+1])
    NETTO_MIG_K2<-c(Netto_K[,k+1])

    N2b_M<-N2a_M+Inflyttade_M-Utflyttade_M+NETTO_MIG_M2
    N2b_K<-N2a_K+Inflyttade_K-Utflyttade_K+NETTO_MIG_K2
    FoddaT2_M<-sum(N2b_K*c(Fx[,k+1]*(1-0.486)))
    FoddaT2_K<-sum(N2b_K*c(Fx[,k+1]*(0.486)))
    N2c_M<-c(FoddaT2_M,N2b_M)
    N2c_K<-c(FoddaT2_K,N2b_K)

    Fx<-rbind(Fx,rep(0,Kol-1))
    Inrikes_Inflyttandel_M<-rbind(Inrikes_Inflyttandel_M,rep(0,Kol-1))
    Inrikes_Inflyttandel_K<-rbind(Inrikes_Inflyttandel_K,rep(0,Kol-1))
    Lokal_Utflyttandel_M<-rbind(Lokal_Utflyttandel_M,rep(0,Kol-1))
    Lokal_Utflyttandel_K<-rbind(Lokal_Utflyttandel_K,rep(0,Kol-1))
    Riket_just_M<-rbind(Riket_just_M,rep(0,Kol-1))
    Riket_just_K<-rbind(Riket_just_K,rep(0,Kol-1))
    Netto_K<-rbind(Netto_K,rep(0,Kol-1))
    Netto_M<-rbind(Netto_M,rep(0,Kol-1))
    Px_M1<-rbind(Px_M1,rep(0.3,Kol-1))
    Px_K1<-rbind(Px_K1,rep(0.3,Kol-1))


    livslangd_M[[i]]<-funktion_medellivslangd(Px_M2)
    my.list_doda_M[[i]]<-Px_M3
    my.list_doda_K[[i]]<-Px_K3
    my.list_M[[ i ]] <- c(N2c_M)
    my.list_K[[ i ]] <- c(N2c_K)
    my.list_TOT[[ i ]] <- c(N2c_M+N2c_K)
    my.list_fodda_M[[i]]<-c(FoddaT2_M)
    my.list_fodda_K[[i]]<-c(FoddaT2_K)
    my.list_netto_M[[i]]<-c(Inflyttade_M-Utflyttade_M+NETTO_MIG_M2)
    my.list_netto_K[[i]]<-c(Inflyttade_K-Utflyttade_K+NETTO_MIG_K2)
    my.list_brutto_INRIKES_UTF_TOT[[i]]<-c(Utflyttade_M+Utflyttade_K)
    my.list_brutto_INRIKES_INF_TOT[[i]]<-c(Inflyttade_M+Inflyttade_K)
    utland.list_K[[i]]<-NETTO_MIG_M2
    utland.list_M[[i]]<-NETTO_MIG_K2


    N1_M<-N2c_M
    N1_K<-N2c_K

  }

  forandringskomponenter<-cbind(sapply(my.list_netto_K,sum),
                                 sapply(my.list_netto_M,sum),
                                 sapply(my.list_netto_M,sum)+sapply(my.list_netto_K,sum),
                                 sapply(my.list_fodda_M,sum),
                                 sapply(my.list_fodda_K,sum),
                                 sapply(my.list_fodda_M,sum)+sapply(my.list_fodda_K,sum),
                                 sapply(my.list_doda_K,sum),
                                 sapply(my.list_doda_M,sum),
                                 sapply(my.list_doda_K,sum)+sapply(my.list_doda_M,sum),
                                 sapply(my.list_brutto_INRIKES_INF_TOT,sum),
                                 sapply(my.list_brutto_INRIKES_UTF_TOT,sum),
                                 sapply(utland.list_K,sum),
                                 sapply(utland.list_M,sum),
                                 sapply(utland.list_K,sum)+sapply(utland.list_M,sum),
                                 sapply(my.list_netto_M,sum)+sapply(my.list_netto_K,sum)-sapply(utland.list_K,sum)-sapply(utland.list_M,sum),
                                 sapply(my.list_fodda_M,sum)+sapply(my.list_fodda_K,sum)-sapply(my.list_doda_M,sum)-sapply(my.list_doda_K,sum),
                                 sapply(my.list_netto_M,sum)+sapply(my.list_netto_K,sum)+sapply(my.list_fodda_M,sum)+sapply(my.list_fodda_K,sum)-sapply(my.list_doda_M,sum)-sapply(my.list_doda_K,sum))

  colnames(forandringskomponenter) <- c("netmigration.women","netmigration.men", "netmigration.tot",
                                         "birts.boys","births.girls","births.tot","deaths.women","deaths.men","deaths.tot",
                                         "inmigration.domestic","outmigration.domestic","international.netmig.w","international.netmig.m","international.netmig.tot","netmig.domestic","net.births","change.pop")

  rownames(forandringskomponenter) <-rep(YEAR:(YEAR+k+1))

  forandringskomponenter[,1]+forandringskomponenter[,2]+forandringskomponenter[,3]+forandringskomponenter[,4]-forandringskomponenter[,5]-
    forandringskomponenter[,6]

  pop.components <- forandringskomponenter

  tillvaxt<-diff(unlist(lapply(my.list_K,sum))+
                   unlist(lapply(my.list_M,sum)))

  tillvaxt<-c(tillvaxt[1]-(sum(startpop[,2:3])),tillvaxt[2:(NCOL(assumptions)-2)])

  tillvaxt <- as.matrix(tillvaxt)

  rownames(tillvaxt) <- rep(YEAR:(YEAR+NCOL(assumptions)-3))
  colnames(tillvaxt) <- "growth"

  my.list_K[[1]]<-N0_K
  PROGNOS_KVINNOR<-unlist(lapply(my.list_K, "length<-", max(lengths(my.list_K))))

  YEAR<-YEAR
  SLUTAR<-YEAR+k+1
  s<-c(YEAR:SLUTAR)

  my.list_M[[1]]<-N0_M
  PROGNOS_MAN<-unlist(lapply(my.list_M, "length<-", max(lengths(my.list_M))))

  my.list_K[[1]]<-N0_K
  PROGNOS_KVINNOR<-unlist(lapply(my.list_K, "length<-", max(lengths(my.list_K))))

  m<-k+101
  m1<-rep((0:m),n+1)
  m2<-rep(YEAR:SLUTAR, each = m+1)

  TOTALT<-PROGNOS_MAN+PROGNOS_KVINNOR
  PROGNOSRESULTAT <- cbind(m1,PROGNOS_MAN,PROGNOS_KVINNOR,TOTALT,m2)

  colnames(PROGNOSRESULTAT) <- c("age","men","women","total","year")
  PROGNOSRESULTAT_2 <- as.data.frame(PROGNOSRESULTAT)

  breaks1<- c(0,1,6,10,13,16,20,25,45,65,80,85,90,1000)
  breaks2<- c(0,seq(0:99),1000)
  agegroup <- cut(PROGNOSRESULTAT_2$age,breaks=breaks1,right=FALSE,
                  include.lowest = FALSE)
  agegroup2 <- cut(PROGNOSRESULTAT_2$age,breaks=breaks2,right=FALSE,
                   include.lowest = FALSE)
  pop.results <- cbind(PROGNOSRESULTAT_2,agegroup,agegroup2)

  rownames(pop.results) <- c(1:NROW(pop.results))
  pop.results <- pop.results



  grupp1 <- dplyr::group_by(pop.results,year,agegroup2,agegroup)
  men <- NULL
  women <- NULL
  year <- NULL
  total <- NULL

  pop.results <- dplyr::summarise(grupp1,
                            total_2 = sum(total,na.rm=TRUE),
                            men_2 = sum(men,na.rm=TRUE),
                            women_2 = sum(women,na.rm=TRUE))

  age.n<-rep(0:100,NCOL(assumptions)-1)
  pop.results <- cbind(as.data.frame(pop.results),as.data.frame(age.n))

return(pop.components)

}



