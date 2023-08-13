# creamos función que chequea si la librería está instalada antes de cargarla. Si es necesario la instala

instalar = function (libreria) {
  if (libreria %in% installed.packages()[,"Package"]) {
    eval(parse(text=paste0("library(",libreria,")")))} else {
      install.packages(libreria)    
      eval(parse(text=paste0("library(",libreria,")")))
    library(libreria)
  }
}

instalar("dplyr")
instalar("glue")
instalar("foreign")
instalar("stringr")

##### Crear tabla de códigos / nombres

# Vamos a crear una tabla con los códigos de cada departamento.
# El indec publica estas tablas de códigos geográficos en formato .pdf lo que imposibilita trabajar esta información con softwares de datos.
# Una solución posible es extraer la codificación y descripción de cada área grográfica de la cartografía digital que disponibiliza el instituto en formato shape.
# La cartografía en formato shape está compuesta por una carpeta que contiene varios archivos con información de diferente tipo. Uno de ellos tiene formato .dbf y almacena la información que permite vincular cada polígono del mapa a un conjunto de datos, por lo cual requiere de tener cada polígono (que en este caso representa a un departamento) debidamente identificado.

# Para comenzar, debemos descargar la cartografía de Argentina dividida en departamentos.
url = "https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_dpto_con_datos.zip"
download.file(url, destfile = "mapa_deptos.zip")

# La carpeta descargada se encuentra comprimida en un archivo .zip, por lo que debemos descomprimirla en una nueva carpeta que llamaremos 'mapa_deptos'.
unzip("mapa_deptos.zip",exdir = "mapa_deptos")
unlink("mapa_deptos.zip")

# Ahora vamos a buscar el nombre del archivo .dbf que contiene la carpeta
nombres_archivos = list.files("mapa_deptos")
nombre_dbf = nombres_archivos[grep("dbf",nombres_archivos)]

# Una vez identificado el archivo lo importamos en un data frame
codigos = foreign::read.dbf(glue("mapa_deptos/{nombre_dbf}"))

# La columna 'link' contiene la información que necesitamos. Las primeras dos posiciones representan el código de la jurisdicción y las tres siguientes el del departamento. Generamos la tabla de códigos, incluyendo los nombres de jurisdicción y departamento.
codigos = data.frame(
  juri_codigo = substring(codigos$link,1,2),
  juri_nombre = codigos$provincia,
  departamento_codigo = substring(codigos$link,3,5),
  departamento_nombre = codigos$departamen) %>% arrange (juri_codigo,departamento_codigo)

# Debemos asegurarnos que los nombres de juisdicciones y departamentos sean iguales en la cartografía y en las proyecciones para poder matchear los datos e incorporar los códigos de departamento a los archivos de proyecciones (que solo contienen la denominación). Para facilitar este matcheo, vamos a eliminar la palabra "Comuna" en los nombres de departamento de la CABA, para representarlos de la misma forma que en la otra fuente de datos.
codigos$departamento_nombre = str_replace_all(codigos$departamento_nombre, "Comuna ","")

##### Procesar datos de proyecciones disponibles en la página del INDEC.

# Las proyecciones por departamento se presentan en 24 archivos (uno para cada jurisdicción).
# En primer lugar vamos a descargar los archivos pogramáticamente. Para esto vamos  necesitar extraer los links a esos archivos y descargarlo.
# Extraer links de una página web es una tarea relativamente fácil que puede hacerse con el paquete rvest. Por ejemplo, podemos extraer todos los links del portal de datos abiertos de Argentina. 

library(rvest)
url = 'https://untref.edu.ar/'
pagina_web = read_html(url)
links = pagina_web %>% html_nodes("a") %>% html_attr("href") 
links = links[substring(links,1,4)== "http" & is.na(links)==F] 
head(links)

# Para el caso de la web del INDEC esta metodología sencilla no funciona, ya que los links aparecen ocultos debido a la forma en la que está programada la página. Por eso, debemos usar la librería webdriver que nos permite ejecutar un navegador virtual que cargue la página por completo y extraiga el código html tal cual lo muestra el navegador, de la misma forma que lo haríams manualmente con Google Chrome usando la opción guardar como html la página.

instalar("webdriver")
pjs <- run_phantomjs()
ses <- Session$new(port = pjs$port)
ses$go("https://www.indec.gob.ar/indec/web/Nivel4-Tema-2-24-119")

Sys.sleep(5)

codigo_html=ses$getSource()

links = strsplit(codigo_html,"\n")[[1]][grep("ftp",strsplit(codigo_html,"\n")[[1]])]
links = unlist(strsplit(links,"<"))[substring(unlist(strsplit(links,"<")),1,7)=="a class"]
links = unlist(strsplit(links,"/"))[substring(unlist(strsplit(links,"/")),1,4)=="proy"]
links1 = unlist(strsplit(links," "))[substring(unlist(strsplit(links," ")),1,4)=="proy"]
links1 = paste0("https://www.indec.gob.ar/ftp/cuadros/poblacion/",stringr::str_remove_all(links1,'\"'))
links = unlist(strsplit(links,">"))[substring(unlist(strsplit(links,">")),1,4)!="proy"]
links = gsub("\\s+$", "", links)

links = data.frame(juri = links, links=links1)
head(links)

# Descargamos los archivos

dir.create("archivos_proyecciones")

for (i in 1:nrow(links)) {
  jurisdiccion = str_replace_all(links[i,1],".xls","")
  download.file(links[i,2], dest = glue("archivos_proyecciones/{jurisdiccion}.xls"), mode="wb")
}

# Con los archivos descargados, vamos a generar un loop que importe cada uno y extraiga la información para almacenarla en un data frame.
# Para eso comenzaremos con generar un vector con todas las rutas a los archivos que importaremos.

rutas_a_archivos = paste0("archivos_proyecciones/",list.files("archivos_proyecciones"))

# De la exploración de los archivos podemos extraer algunos valores constantes que nos ayudarán a recopilar la información.
# 1- Cada archivo tiene 3 bloques de información: proyecciones para "ambos sexos", "varones", y "mujeres"
# 2- Todos los bloques tienen una columna con los nombres de los departamentos y 16 columnas (desde la B a la Q) con los valores de población
# 3- El primer bloque comienza en la fila 10 y en la fila 5 se indica el sexo correspondiente al bloque. Si bien el comienzo del segundo bloque depende de la cantidad de departamentos de la provincia que tomemos, en los siguientes bloques también se encuentra el sexo 5 filas antes del comienzo.
# 4- Los bloques 2 y  3 comienzan 10 filas debajo de su predecesor.
# 4- El archivo de la Provincia de Buenos Aires presenta una estructura levemente diferente ya que los departamentos están agrupados en GBA y resto de la provincia.

# Con esta situación descripta, ver de qué forma obtenemos la longitud de celdas de cada bloque, que representa la única incognita restante para poder obtener los datos

instalar("readxl")
proyecciones_depto = data.frame()


for (i in 1:length(rutas_a_archivos)) {
  incluir=T
  for (j in 9:500) {
    prov = str_remove_all(rutas_a_archivos[i],"archivos_proyecciones/")
    prov = str_remove_all(prov,".xls")
    print(prov)
    comienzo_bloque_1 = if (prov %in% c("Buenos Aires","La Rioja", "Río Negro","Salta", "San Juan",
                                        "San Luis","Santa Cruz","Santa Fe","Santiago del Estero",
                                        "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
                                        "Tucumán")) {11} else {10}
    celda = comienzo_bloque_1-1+j
    valor=as.character(read_xls(rutas_a_archivos[i], range = glue("A{celda}:A{celda}"), col_names = F))
    if (length(valor)>0) {if(valor=="Total") {break}} 
  }
  
  #browser(expr = {prov=="La Pampa"}) 
  if (prov == "Buenos Aires") {
    fin_bloque_1 = comienzo_bloque_1 + j -9
    comienzo_bloque_2 = fin_bloque_1 + 10
    fin_bloque_2 = comienzo_bloque_2 + j -8
    comienzo_bloque_3 = fin_bloque_2 + 10
    fin_bloque_3 = comienzo_bloque_3 + j -8
  } else if (prov == "La Pampa") {
    fin_bloque_1 = comienzo_bloque_1 + j -9
    comienzo_bloque_2 = fin_bloque_1 + 10
    fin_bloque_2 = comienzo_bloque_2 + j -9
    comienzo_bloque_3 = fin_bloque_2 + 11
    fin_bloque_3 = comienzo_bloque_3 + j -9
  } else if (prov %in% c("La Rioja","Río Negro","Salta")) {
    fin_bloque_1 = comienzo_bloque_1 + j -10
    comienzo_bloque_2 = fin_bloque_1 + 11
    fin_bloque_2 = comienzo_bloque_2 + j -10
    comienzo_bloque_3 = fin_bloque_2 + 11
    fin_bloque_3 = comienzo_bloque_3 + j -10
  } else {
    fin_bloque_1 = comienzo_bloque_1 + j -9
    comienzo_bloque_2 = fin_bloque_1 + 10
    fin_bloque_2 = comienzo_bloque_2 + j -9
    comienzo_bloque_3 = fin_bloque_2 + 10 
    fin_bloque_3 = comienzo_bloque_3 + j -9
    
  }
  
  
  datos = rbind(
    read_xls(rutas_a_archivos[i], range = glue("B{comienzo_bloque_1}:Q{fin_bloque_1}"), col_names = F),
    read_xls(rutas_a_archivos[i], range = glue("B{comienzo_bloque_2}:Q{fin_bloque_2}"), col_names = F),
    read_xls(rutas_a_archivos[i], range = glue("B{comienzo_bloque_3}:Q{fin_bloque_3}"), col_names = F)
  )
  
  colnames(datos) = paste0("a_",2010:2025)
  datos = datos[is.na(datos$a_2010)==F,]
  
  datos$juri_nombre = prov
  
  datos$sexo_nombre = c(rep("Ambos sexos", nrow(datos)/3),
                        rep("Varones", nrow(datos)/3),
                        rep("Mujeres", nrow(datos)/3))
  departamento_nombre = unique(read_xls(rutas_a_archivos[i], range = glue("A{comienzo_bloque_1}:A{fin_bloque_3}")))
  departamento_nombre = departamento_nombre[is.na(departamento_nombre)==F]
  departamento_nombre = departamento_nombre[departamento_nombre!="Interior de la Provincia"]
  departamento_nombre = departamento_nombre[departamento_nombre!="24 Partidos del GBA"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Varones"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Mujeres"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Total"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Partido"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Departamento"]
  departamento_nombre = departamento_nombre[departamento_nombre!="Comuna"]
  
  datos$departamento_nombre = rep(departamento_nombre,3)
  datos = datos[,c("juri_nombre","departamento_nombre", "sexo_nombre",colnames(datos)[substring(colnames(datos),1,2)=="a_"])]
  proyecciones_depto = rbind(
    proyecciones_depto,
    datos
  )
}

url_prov = "https://www.indec.gob.ar/ftp/cuadros/poblacion/c2_proyecciones_prov_2010_2040.xls"
system(glue('curl -o poblacion.xls {url_prov}'))

total_argentina_2010_prov = as.numeric(colnames(read_xls("poblacion.xls", sheet= "01-TOTAL DEL PAÍS",range = "B6:D6", col_names = T)))
names(total_argentina_2010_prov) = c("Ambos sexos","Varones","Mujeres")

total_argentina_2010_depto = c(ambos_sexos=sum(proyecciones_depto$a_2010[proyecciones_depto$sexo_nombre=="Ambos sexos"]),
                               varones=sum(proyecciones_depto$a_2010[proyecciones_depto$sexo_nombre=="Varones"]),
                               mujeres=sum(proyecciones_depto$a_2010[proyecciones_depto$sexo_nombre=="Mujeres"]))

total_argentina_2010_depto == total_argentina_2010_prov

proyecciones_depto = as.data.frame(proyecciones_depto)

proyecciones_depto$juri_nombre[proyecciones_depto$juri_nombre=="Tierra del Fuego, Antártida e Islas del Atlántico Sur"] = "Tierra del Fuego"
proyecciones_depto$departamento_nombre[proyecciones_depto$departamento_nombre=="Ñorquincó"] = "Ñorquinco"

proyecciones_depto = left_join(proyecciones_depto,codigos, by = c("juri_nombre","departamento_nombre"))

proyecciones_depto[is.na(proyecciones_depto$departamento_codigo),]
