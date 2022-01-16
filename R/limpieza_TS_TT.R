#-------------------------------------------------------------------------------#
#----------- {DApers}: FUNCIONES PARA DEPURAR DATOS (desde 13/01/22) -----------#
#-------------------------------------------------------------------------------#

### FUNCION importar_tt 12/01/22

#' Importa formato TT y lo apronta para TS
#'
#' Partiendo de archivo_csv (corregido) se hacen depuraciones adicionales para
#' conformar al formato TT
#'
#' Fundamental haber corregido CSV ANTES: edicion MANUAL previa de archivo TT
#' (agregar comillas necesarias, controlar '1 fila 1 registro', guardar con separador TAB)
#'
#' Es la parte 1 del proceso importar_tt() >> etiq_rp(2): crear RP20xx
#'
#' @param archivo_csv ubicacion del archivo CSV YA CORREGIDO
#' @param ... No usado
#' @export

importar_tt <- function(archivo_csv,...){
  #unit_testing DE ENTRADA!!
  #Instructivo claro
  tt <- readLines(archivo_csv,...)
  split <- strsplit(tt,split="\t",fixed=TRUE)
  df_tt <- as.data.frame(do.call("rbind",lapply(split,"[",1:8)),stringsAsFactors=FALSE)
  df_tt$V9 <- lapply(split,"[",-(1:8))
  nmb <- split[[1]]
  tt <- data.frame(df_tt[-1,],stringsAsFactors=FALSE)
  colnames(tt) <- nmb
  names(tt) <- sub(" ","",names(tt)) #saco espacios del principio de cada columna
  names(tt) <- gsub(" ",".",names(tt)) #cambio espacios por puntos para hms_fch()
  names(tt) <- gsub("\"","",names(tt)) #me agrega "" en nombres! Las pelamos
  tt[,9] <- as.character(tt[,9]) #$` COMMENTS` list a character
  #No sabemos porque se agregan comillas dobles, las sacamos
  tt$ACTIVITY.NAME <- gsub("\"","",tt$ACTIVITY.NAME) #noquote(tt$ACTIVITY.NAME)
  tt$COMMENTS <- gsub("\"","",tt$COMMENTS)
  tt[which(tt[,9]=="character(0)"),9] <- NA
  return(tt)
}

### FUNCION hms_fch para pasar lectura datos TT>>TS (corregida 06/04/20) ###

#' Equivalencia de fechas TS-TT
#'
#' Cambia formato de fechas, dias, horas, etc. para hacerlo compatible con TS
#'
#' Usada hasta 2019 con frecuencia; se deja de usar en 2021 por mejora en etiq_rp()
#'
#'
#' @param df_tt algo
#' @param formatoFecha 1:version <2019, 2:version>2019 (x defecto)
#' @param formatoTS No usado
#' @param exportar (TRUE por defecto) si queres exportar data.frame resultante
#' @export

hms_fch <- function(df_tt,formatoFecha='%d/%m/%Y',formatoTS=2,exportar=TRUE){
  #formatoTS: 1:version <2019, 2:version>2019 (x defecto)
  a <- df_tt
  # Parte 1: Funcion 'hms': entra df TT, sale df TT+ columna 'DURATIONrec' con horas en formato 'H.xx'
  #b1 <- data.frame(t(data.frame(strsplit(x=as.character(a$DURATION),split=":")))) #cambiado 08/01/21
  b1 <- data.frame(Reduce(rbind,strsplit(x=as.character(a$DURATION),split=":")))
  colnames(b1) <- c("H","M","S")
  #(16/06/22)Agregamos por comportamiento diferente en Linux vs Windows
  if(.Platform$OS.type=="linux"){#como ants
    scomplem <- ifelse(as.numeric(b1$S)-1<=30,0,1)
    b1$msdec <- round(((as.numeric(b1$M)-1)+scomplem)/60,2)
    b1$hmsdec <- (as.numeric(b1$H)-1) + b1$msdec
  }
  if(.Platform$OS.type=="windows"){#windows resta 1h 1m a cada registro
    scomplem <- ifelse(as.numeric(b1$S)<=30,0,1)
    b1$msdec <- round(((as.numeric(b1$M))+scomplem)/60,2)
    b1$hmsdec <- (as.numeric(b1$H)) + b1$msdec
  }
  a$DURATIONdec <- b1$hmsdec #col09
  # Parte 2: Funcion 'fch': entra df TT, sale df TT+ columnas 'Fch','coment_hm' con fecha de inicio y horas como etiqueta en comentarios
  #Version actual (>2019): '%d/%m/%y'; version anterior (<2019): '%d %b. %Y'
  fch_ini <- format(as.POSIXct(strptime(as.character(a$START.TIME),format=formatoFecha,tz=""),format="%Y-%m-%d"))
  fch_fin <- format(as.POSIXct(strptime(as.character(a$END.TIME),format=formatoFecha,tz=""),format="%Y-%m-%d"))
  coment_hm_etq <- a$ACTIVITY.NAME
  coment_hm_ini <- substr(as.POSIXct(strptime(as.character(a$START.TIME),format=paste0(formatoFecha,' %H:%M'),tz="")),start=12,stop=16)
  coment_hm_fin <- substr(as.POSIXct(strptime(as.character(a$END.TIME),format=paste0(formatoFecha,' %H:%M'),tz="")),start=12,stop=16)
  coment_hm <- paste("H:m para ",coment_hm_etq,":",coment_hm_ini,"-",coment_hm_fin,sep="")
  a$Fch <- fch_ini
  a$HrIni <- coment_hm_ini
  a$HrFin <- coment_hm_fin
  a$coment_hm <- ifelse(is.na(a$COMMENTS),coment_hm,a$COMMENTS) #cambiado 14/01/22
  # Parte 3: formatear estilo TS pero sacando tambien la info desde ACTIVITY.NAME (modif 14/01/22)
  a$proye <- factor(with(a,ifelse(ACTIVITY.NAME %in% c("ESIN-R.impl","ESRE-DAb","ESTE-Adm","ESTE-Bib","ESTE_Reu"),"Estudio-Tesis",
                ifelse(ACTIVITY.NAME %in% c("ESMA-TM"),"Estudio-Materias",
                ifelse(ACTIVITY.NAME %in% c("OH-Auto","OH-Coci","OH-CoFe","OH-Ctas","OH-LiLR","OH-Limp","OH-Mand","OH-OrgL","OH-ObjC"),"Otros-Hogar",
                ifelse(ACTIVITY.NAME %in% c("FA-CuiF","FA-DocF","FA-VEFP","FA-ApyF","FA-EveF"),"Familia",
                ifelse(ACTIVITY.NAME %in% c("OP-DocP","OP-Mand","OP-Salu","OP-CuiP","OP-HNTI","OP-ObjP","OP-Plan","OP-SaCt","OP-Varios","OP-DebC"),"Otros-Personales",
                ifelse(ACTIVITY.NAME %in% c("TL-EC","TL-Otr","TL-ReAm","TL-ReFa","TL-Sali","TL-Siestas","TL-Viaj"),"Tiempo_Libre",
                ifelse(ACTIVITY.NAME %in% c("Trp-Bus","Trp-Priv","Trp-Taxi","Trp-Intl"),"Transporte",
                #Agregado 13/01/21 por no estar antes en TT
                ifelse(ACTIVITY.NAME %in% c("TRFG-ACUy","TRFG-Cortos","TRFG-PSV","TRFG-PW","TRFG-SVD"),"Trabajo-FGR",
                ifelse(ACTIVITY.NAME %in% c("TRUE-Ap","TRUE-CU","TRUE-In","TRUE-Ot","TRUE-Ge","TRUE-Gral","TRUE-Re"),"Trabajo-UEFI",
                ifelse(ACTIVITY.NAME %in% c("TRNF"),"Trabajo-NoFacturable",
                ifelse(ACTIVITY.NAME %in% c("TRAS"),"Trabajo-Asesoramiento",
                #Nuevos desde 14/01/22
                ifelse(ACTIVITY.NAME %in% c("DIEC","DICC"),"Deporte",
                ifelse(ACTIVITY.NAME %in% c("TRIM"),"Trabajo-IM","__Completar__")))))))))))))))
  # Parte 4: reordenamos columnas con nombres correctos (y agregando columnas vacias para que se parezca a TS)
  a$DurRel <- a$DURATIONdec #iguales pa que no se arme lio
  a$GastosDscr <- a$GastosNoPag <- a$GastosPag <- a$Gastos <- a$PausasDscr <- a$Pausas <- a$Salario <- 0
  a$Notas <- ""
  nuevo_a <- a[,c(11,12,13,10,16,17,15,14,6,18:24)] #antes: 10:12,9,15:16,14,13,6,17:23
  names(nuevo_a)[c(4,7:9)] <- c("Duracion","Proyecto","Descripcion","Etiquetas")
  # Final: devuelvo nuevo DF con 3 cols extra
  if(formatoTS==1){
    #Exportar archivo
    if(exportar){
      write.table(nuevo_a,"TT_modif_R.txt",sep="\t")
    }
    return(nuevo_a)
  }
  else if (formatoTS==2){
    #Fecha|Hora de inicio|Hora de fin|Duración|Duración rel.|Proyecto|Descripción|Etiquetas|Notas
    an <- nuevo_a[,c(1:5,7:9,16)]
    #Exportar archivo
    if(exportar){
      write.table(an,"TT_modif_R.txt",sep="\t")
    }
    return(an)
  }
  else return(nuevo_a) #x otros formatos modificar esta parte
}


### FUNCION etiq_rp 15/01/21

#' Genera tabla con formato TS
#'
#' Con salida de importar_tt() se crea nuevo data.frame ajustado a formato TS
#'
#' Es la parte 2 del proceso importar_tt() >> etiq_rp(2): crear RP20xx
#'
#' @param datos algo
#' @param tablaEtiq Tabla de Equivalencias de etiquetas TS-TT
#' @param nombre Nombre al nuevo data.frame
#' @param exportar (TRUE por defecto) si queres exportar data.frame resultante
#' @export

etiq_rp <- function(datos,tablaEtiq,nombre="",exportar=TRUE){
  rp <- datos
  if(is.null(tablaEtiq))
    stop("NO se puede seguir sin tabla de equivalencias entre etiquetas TS-TT")
  rp2 <- merge(rp,tablaEtiq[,1:7],by="Etiquetas",all.x=TRUE,all.y=FALSE,sort=FALSE)
  #Ordenamos por fecha ascendente
  rp2 <- rp2[,c(2,10:14,8,5,3:4,1)] #cambio 14/01/22 (antes 2,9:13,8,5,3:4,1)
  rp2 <- rp2[order(rp2[,1]),]
  #Retocamos fecha como antes
  fch <- strftime(rp2$Fch)
  #Mes
  ms <- strftime(fch,format='%b')
  rp2$Mes <- ifelse(ms %in% c("ene","Ene","ene.","Ene."),"A.Ene",
             ifelse(ms %in% c("feb","Feb","feb.","Feb."),"B.Feb",
             ifelse(ms %in% c("mar","Mar","mar.","Mar."),"C.Mar",
             ifelse(ms %in% c("abr","Abr","abr.","Abr."),"D.Abr",
             ifelse(ms %in% c("may","May","may.","May."),"E.May",
             ifelse(ms %in% c("jun","Jun","jun.","Jun."),"F.Jun",
             ifelse(ms %in% c("jul","Jul","jul.","Jul."),"G.Jul",
             ifelse(ms %in% c("ago","Ago","ago.","Ago."),"H.Ago",
             ifelse(ms %in% c("set","sep","sept","Set",
                              "Sep","Sept","set.","sep.",
                              "sept.","Set.","Sep.","Sept."),"I.Set", #OJO con el LOCALE (si es UY es SETiembre!!)
             ifelse(ms %in% c("oct","Oct","oct.","Oct."),"J.Oct",
             ifelse(ms %in% c("nov","Nov","nov.","Nov."),"K.Nov",
             ifelse(ms %in% c("dic","Dic","dic.","Dic."),"L.Dic",NA))))))))))))
  #Semana
  sm <- as.numeric(strftime(fch,format='%U'))
  rp2$Semana <- ifelse(sm<9,paste0("Sem0",sm+1),paste0("Sem",sm+1)) #valido pa 2018 (en 2017 sacar el '+1')
  #DdS
  dd <- strftime(fch,format='%a')
  rp2$DdS <- ifelse(dd %in% c("dom","dom."),"a.dom",
              ifelse(dd %in% c("lun","lun."),"b.lun",
              ifelse(dd %in% c("mar","mar."),"c.mar",
              ifelse(dd %in% c("mi\u00E9","mi\u00E9."),"d.mie",
              ifelse(dd %in% c("jue","jue."),"e.jue",
              ifelse(dd %in% c("vie","vie."),"f.vie",
              ifelse(dd %in% c("s\u00E1b","s\u00E1b."),"g.sab",NA)))))))
  #Cambiamos nombre cols y reordenamos:
  names(rp2)[c(1,5:10)] <- c("Fecha","Sub_espacio","Sub_subespacio","Descripcion", "Tiempo","HoraInicio","HoraFin")
  rp2 <- rp2[c("Mes","Semana","DdS","Fecha","TA","Actividad","Espacio","Sub_espacio","Sub_subespacio",
               "Descripcion","Tiempo","HoraInicio","HoraFin","Etiquetas")]
  # Paso 5: exportar archivo para guardar en GSS (max info, min t perdido!)
  if(exportar){
    write.table(rp2,file=paste0(nombre,".txt"),sep="\t",row.names=FALSE)
  }
  # Final: devolveme rp2 pa trabajar
  return(rp2)
}


#-------------------------------------------------------------------------------#

# Fuente: https://r-pkgs.org/man.html

# Titulo
#
# Descripcion
#
# Detalle (opcional): descripcion larga del objeto
#
# Para FUNCIONES:
# @param nombre_parametro Descripcion de lo que hace
# @return Descripcion de lo que devuelve la funcion
# @examples Ejemplos concretos de uso

