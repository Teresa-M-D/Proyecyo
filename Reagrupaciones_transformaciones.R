#Reagrupaciones y transformaciones lineales:

#Transformacion lineal notas sobre 200 a sobre 10:

#Previos qualification grade (nota de estudios previos):
datos_recodificados$Previous.qualification.grade_10 <- (datos_recodificados$Previous.qualification..grade. / 20)

#Admission grade (nota de adimisión):
datos_recodificados$Admission.grade_10 <- (datos_recodificados$Admission.grade / 20)

#Reagrupaciones:

#Reagrupamos Nacionalidades en categorías Portugal, Europa, África y América Latina

table(datos_recodificados$Nationality)


datos_recodificados$Nationality_group <- ifelse(
  datos_recodificados$Nationality == "Portugués", "Portugal",
  
  ifelse(datos_recodificados$Nationality %in% c("Español", "Alemán", "Italiano", "Neerlandés", "Inglés", "Lituano", "Rumano", "Ruso", "Turco", "Ucraniano", "Moldavo (República de Moldavia)"),
         "Europa",
         
         ifelse(datos_recodificados$Nationality %in% c("Brasileño", "Mexicano", "Cubano", "Colombiano"),
                "América Latina",
                
                ifelse(datos_recodificados$Nationality %in% c("Angoleño", "Caboverdiano", "Guineano", "Mozambiqueño", "Santotomense"),
                       "África",
                       NA
                )
         )
  )
)




#Previous qualification:
table(datos_recodificados$Previous.qualification)
datos_recodificados$Previous_education_level <- case_when(
  
  # BAJO
  datos_recodificados$Previous.qualification %in% c(
    "10º curso", 
    "10º curso - no completado",
    "11º curso - no completado",
    "12º curso - no completado",
    "Educación básica 2º ciclo (6º/7º/8º) o equivalente",
    "Educación básica 3er ciclo (9º/10º/11º) o equivalente",
    "Otro - 11º curso"
  ) ~ "Bajo",
  
  # MEDIO
  datos_recodificados$Previous.qualification %in% c(
    "Educación secundaria"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_recodificados$Previous.qualification %in% c(
    "Curso de especialización tecnológica",
    "Curso técnico superior profesional"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_recodificados$Previous.qualification %in% c(
    "Educación superior - grado",
    "Educación superior - grado (1er ciclo)",
    "Educación superior - grado (bachelor)",
    "Educación superior - máster",
    "Educación superior - máster (2º ciclo)",
    "Educación superior - doctorado",
    "Asistencia a educación superior"
  ) ~ "Superior"
)

descriptive(datos_recodificados)
table(datos_recodificados$Previous_education_level)

#Mother qualification en nivel bajo, medio, técnico y superior
table(datos_recodificados$Mother.s.qualification)


datos_recodificados$Mother_education_level <- case_when(
  
  # BAJO
  datos_recodificados$Mother.s.qualification %in% c(
    "10º año de escolarización", 
    "11º año de escolarización - No completado",
    "12º año de escolarización - No completado",
    "7º año (sistema antiguo)",
    "7º año de escolarización",
    "8º año de escolarización",
    "9º año de escolarización - No completado",
    "Educación básica 1er ciclo (4º/5º año) o equivalente",
    "Educación básica 2º ciclo (6º/7º/8º año) o equivalente",
    "Educación básica 3er ciclo (9º/10º/11º año) o equivalente",
    "Otro - 11º año de escolarización",
    "Sabe leer sin haber completado 4º año",
    "No sabe leer ni escribir"
  ) ~ "Bajo",
  
  # MEDIO
  datos_recodificados$Mother.s.qualification %in% c(
    "Educación secundaria - 12º año o equivalente",
    "2º ciclo del bachillerato general"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_recodificados$Mother.s.qualification %in% c(
    "Curso de especialización tecnológica",
    "Curso de estudios superiores especializados",
    "Curso técnico-profesional",
    "Curso técnico superior profesional",
    "Curso general de comercio"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_recodificados$Mother.s.qualification %in% c(
    "Educación superior - Doctorado",
    "Educación superior - Doctorado (3er ciclo)",
    "Educación superior - Grado",
    "Educación superior - Grado (Bachelor)",
    "Educación superior - Máster",
    "Educación superior - Máster (2º ciclo)",
    "Educación superior - Grado (1er ciclo)",
    "Asistencia a educación superior"
  ) ~ "Superior"
)

table(datos_recodificados$Mother_education_level)

#Father qualification en nivel bajo, medio, técnico y superior
table(datos_recodificados$Father.s.qualification)

datos_recodificados$Father_education_level <- case_when(
  # BAJO
  datos_recodificados$Father.s.qualification %in% c(
    "10º año de escolarización", 
    "11º año de escolarización - No completado",
    "12º año de escolarización - No completado",
    "7º año (sistema antiguo)",
    "7º año de escolarización",
    "8º año de escolarización",
    "9º año de escolarización - No completado",
    "Educación básica 1er ciclo (4º/5º año) o equivalente",
    "Educación básica 2º ciclo (6º/7º/8º año) o equivalente",
    "Educación básica 3er ciclo (9º/10º/11º año) o equivalente",
    "Otro - 11º año de escolarización",
    "Sabe leer sin haber completado 4º año",
    "No sabe leer ni escribir"
  ) ~ "Bajo",
  
  # MEDIO
  datos_recodificados$Father.s.qualification %in% c(
    "Educación secundaria - 12º año o equivalente",
    "2º ciclo del bachillerato general"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_recodificados$Father.s.qualification %in% c(
    "Curso de especialización tecnológica",
    "Curso de estudios superiores especializados",
    "Curso técnico-profesional",
    "Curso técnico superior profesional",
    "Curso general de comercio",
    "Curso general de administración y comercio",
    "Curso complementario de contabilidad y administración",
    "Curso complementario de secundaria",
    "Curso complementario de secundaria - no completado",
    "2º año de curso complementario de secundaria"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_recodificados$Father.s.qualification %in% c(
    "Educación superior - Doctorado",
    "Educación superior - Doctorado (3er ciclo)",
    "Educación superior - Grado",
    "Educación superior - Grado (Bachelor)",
    "Educación superior - Máster",
    "Educación superior - Máster (2º ciclo)",
    "Educación superior - Grado (1er ciclo)",
    "Asistencia a educación superior"
  ) ~ "Superior"
)
table(datos_recodificados$Father_education_level)


#Mother occupation

table(datos_recodificados$Mother.s.occupation)

datos_recodificados$Mother_occupation_level <- case_when(
  
  # ALTA CUALIFICACIÓN
  datos_recodificados$Mother.s.occupation %in% c(
    "Representantes del poder legislativo y ejecutivo, directores y gerentes",
    "Especialistas en actividades intelectuales y científicas",
    "Especialistas en tecnologías de la información y la comunicación (TIC)",
    "Profesionales de la salud",
    "Profesores"
  ) ~ "Alta cualificación",
  
  # CUALIFICACIÓN MEDIA
  datos_recodificados$Mother.s.occupation %in% c(
    "Personal administrativo",
    "Empleados de oficina, secretarios y operadores de datos",
    "Operadores de datos, contabilidad, estadística, servicios financieros y registros",
    "Otro personal de apoyo administrativo",
    "Técnicos y profesiones de nivel intermedio",
    "Técnicos y profesiones intermedias en ciencia e ingeniería",
    "Técnicos y profesionales de nivel intermedio en salud",
    "Técnicos intermedios en servicios jurídicos, sociales, deportivos, culturales y similares"
  ) ~ "Cualificación media",
  
  # BAJA CUALIFICACIÓN
  datos_recodificados$Mother.s.occupation %in% c(
    "Trabajadores de servicios personales, seguridad y vendedores",
    "Trabajadores de servicios personales",
    "Vendedores",
    "Trabajadores de cuidado personal y similares",
    "Agricultores y trabajadores cualificados en agricultura, pesca y silvicultura",
    "Trabajadores cualificados de la industria, construcción y artesanos",
    "Trabajadores cualificados de la construcción (excepto electricistas)",
    "Trabajadores cualificados en impresión, instrumentos de precisión, joyería y artesanía",
    "Trabajadores en procesamiento de alimentos, madera, textil y otras industrias",
    "Operadores de instalaciones y maquinaria y trabajadores de montaje",
    "Ayudantes de preparación de comidas"
  ) ~ "Baja cualificación",
  
  # NO CUALIFICADOS
  datos_recodificados$Mother.s.occupation %in% c(
    "Trabajadores no cualificados",
    "Trabajadores no cualificados en agricultura, pesca y silvicultura",
    "Trabajadores no cualificados en industria extractiva, construcción, manufactura y transporte",
    "Trabajadores de limpieza"
  ) ~ "No cualificados",
  
  # OTROS
  datos_recodificados$Mother.s.occupation %in% c(
    "Estudiante",
    "Otra situación",
    "Profesiones de las fuerzas armadas"
  ) ~ "Otros"
  
)
sum(table(datos_recodificados$Mother_occupation_level))
table(datos_recodificados$Mother.s.occupation)

#Father occupation

table(datos_recodificados$Father.s.occupation)


datos_recodificados$Father_occupation_level <- case_when(
  
  # ALTA CUALIFICACIÓN
  datos_recodificados$Father.s.occupation %in% c(
    "Representantes del poder legislativo y ejecutivo, directores y gerentes",
    "Directores de servicios administrativos y comerciales",
    "Directores de hostelería, comercio y otros servicios",
    "Especialistas en actividades intelectuales y científicas",
    "Especialistas en ciencias físicas, matemáticas, ingeniería y afines",
    "Especialistas en finanzas, contabilidad, organización administrativa y relaciones públicas/comerciales",
    "Profesionales de la salud",
    "Profesores"
  ) ~ "Alta cualificación",
  
  # CUALIFICACIÓN MEDIA
  datos_recodificados$Father.s.occupation %in% c(
    "Personal administrativo",
    "Empleados de oficina, secretarios y operadores de datos",
    "Operadores de datos, contabilidad, estadística y servicios financieros",
    "Otro personal de apoyo administrativo",
    "Técnicos en tecnologías de la información y la comunicación",
    "Técnicos y profesiones de nivel intermedio",
    "Técnicos intermedios en ciencia e ingeniería",
    "Técnicos y profesionales intermedios de salud",
    "Técnicos intermedios en servicios jurídicos, sociales, deportivos y culturales"
  ) ~ "Cualificación media",
  
  # BAJA CUALIFICACIÓN
  datos_recodificados$Father.s.occupation %in% c(
    "Trabajadores de servicios personales, seguridad y vendedores",
    "Trabajadores de servicios personales",
    "Vendedores",
    "Vendedores ambulantes (excepto alimentos) y servicios callejeros",
    "Trabajadores de cuidado personal y similares",
    "Personal de protección y seguridad",
    "Agricultores y trabajadores cualificados en agricultura, pesca y silvicultura",
    "Agricultores orientados al mercado y trabajadores agrícolas cualificados",
    "Agricultores de subsistencia, pescadores, cazadores y recolectores",
    "Trabajadores cualificados de la industria, construcción y artesanía",
    "Trabajadores cualificados de la construcción (excepto electricistas)",
    "Trabajadores cualificados en metalurgia y trabajo del metal",
    "Trabajadores cualificados en electricidad y electrónica",
    "Trabajadores en alimentación, madera, textil y otras industrias",
    "Operadores de instalaciones y maquinaria y trabajadores de montaje",
    "Operadores de instalaciones y maquinaria fija",
    "Conductores de vehículos y operadores de maquinaria móvil",
    "Trabajadores de montaje",
    "Ayudantes de preparación de comidas"
  ) ~ "Baja cualificación",
  
  # NO CUALIFICADOS
  datos_recodificados$Father.s.occupation %in% c(
    "Trabajadores no cualificados",
    "Trabajadores no cualificados en agricultura, pesca y silvicultura",
    "Trabajadores no cualificados en industria, construcción y transporte"
  ) ~ "No cualificados",
  
  # OTROS
  datos_recodificados$Father.s.occupation %in% c(
    "Estudiante",
    "Otra situación"
    
  ) ~ "Otros",
  
  # MILITAR
  datos_recodificados$Father.s.occupation %in% c(
    "Profesiones de las fuerzas armadas",
    "Oficiales de las fuerzas armadas",
    "Sargentos de las fuerzas armadas",
    "Otro personal de las fuerzas armadas"
  ) ~ "Formación militar"
  
)

