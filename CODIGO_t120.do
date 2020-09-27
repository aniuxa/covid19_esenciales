* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------
* CÓDIGO PARA EL ESTUDIO DE TRABAJO ESENCIAL 
* TETRA-COYUNTURA, MAYO-SEPTIEMBRE, 2020 //
* AUTORA: ANA ESCOTO
* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------


capture log close
set more off
clear


* -----------------------------------------------------------------------------
*  ESTABLECIENDO DIRECTORIOS
* -----------------------------------------------------------------------------
*sustituir en las siguientes líneas según los directorios de las máquinas
***** 
gl  root "" // carpeta principal de documentos
gl  dta "$root/2020trim1_dta" // carpeta donde está la base de STATA descargada y 
								   // descomprimida desde
* https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/2019trim4_dta.zip
gl log "$root/out"  // donde quiero guardar el log - el log guarda la pantalla de resultados
gl tempdat "$root/out/temp" // donde van los datos temporales que después puedo borrar
gl cleandat "$root/out/clean" // donde están mis datos limpios - pegados
gl docs "$root/out/docs2" // donde se guardarán los outputs
gl gra "$root/out/graphs" // donde se guardarán los gráficos
gl tabout "/Applications/Stata/ado/personal"  // 
gl class "$root/out"  // donde esté la clasificación

* -----------------------------------------------------------------------------
* PAQUETES QUE SE TIENEN QUE INSTALAR
* -----------------------------------------------------------------------------
* Paquetería fuera de stata para instalar. DESCOMENTAR LAS LINEAS

*ssc install renvarlab, replace // la nueva versión el comando se llama renvarlab o no renvars como en el código
*ssc install estout, replace // guarda estimaciones y las exporta 
*ssc install coefplot, replace // gráficos de estimaciones
*ssc install blindschemes, replace all // esquemas de los gráficos
*ssc install grc1leg, replace // combinar gráficos con leyenda común

* -----------------------------------------------------------------------------
* FUSIONAR LA ENOE TRIMESTRE1 2020
* -----------------------------------------------------------------------------

log using "$log/fusionT1_2020.log", replace


* globals con el listado de variables
* folio= id de vivienda
gl xfolio cd_a ent con v_sel		
* folio2= id de hogar
gl xfolio2 $xfolio n_hog h_mud
* folio3= id de individuo
gl xfolio3 $xfolio2 n_ren

*folio viv
	use "$dta/vivt120.dta", clear
	renvars *, lower // en otros trimestres las bases están en mayúsculas.
	egen folio=concat($xfolio)
	isid folio // verfica ID único
	tab t_loc, m
	save "$tempdat/vivt120.dta", replace

*folio hogar
	use "$dta/hogt120.dta", clear
	
	* convertimos los id en cadenas para un uso más ordenado	
	renvars *, lower
	egen folio=concat($xfolio)
	egen folio2=concat($xfolio2)
	isid  folio2 // verifica ID único
	tab t_loc, m
	save "$tempdat/hogt120.dta", replace

	*viv+hogar
	use "$tempdat/vivt120.dta", clear
	merge 1:m folio using "$tempdat/hogt120.dta" 
	drop _merge
	tab t_loc, m
	keep if r_def==0
	tab t_loc, m
	list folio folio2 in 1/3000 if p2==2 // p2=="2": más de un hogar en la vivienda
	save "$tempdat/enoet120_completa.dta", replace

	*folio sdem
	use "$dta/sdemt120.dta", clear
	renvars *, lower
	egen folio2=concat($xfolio2)
	egen folio3=concat($xfolio3)
	isid folio3 

	*sdem+viv+hogar
	keep if r_def==0
	keep if c_res!=2
	merge m:1 folio2 using "$tempdat/enoet120_completa.dta"
	drop _merge
	list folio2 sex in 1/20
	isid folio3
	save "$tempdat/enoet120_completa.dta", replace

	*folio coe1
	use "$dta/coe1t120.dta", clear
	renvars *, lower
	egen folio3=concat($xfolio3)

	isid folio3
	rename p1 p1coe
	rename p3 p3coe
	save "$tempdat/coe1t120.dta", replace

	*folio coe2
	use "$dta/coe2t120.dta", clear
	renvars *, lower
	egen folio3=concat($xfolio3)

	isid folio3
	save "$tempdat/coe2t120.dta", replace

	*viv+hog+sdem+coe1+coe2
	use "$tempdat/enoet120_completa.dta", clear
	merge 1:1 folio3 using "$tempdat/coe1t120.dta" 
	drop _merg
	merge 1:1 folio3 using "$tempdat/coe2t120.dta"
	tab sex,m

gen trim="t2"
gen year=2020
	
save "$cleandat/enoet120completa.dta", replace

log close

* -----------------------------------------------------------------------------
*  VARIABLES ACCESORIAS
* -----------------------------------------------------------------------------

log using "$log/vars2020.log", replace



*2013 en adelante
/*
1estudiar o tomar cursos de capacitación?(incluye el tiempo dedicado  a  realizar  trabajos escolares)
2cuidar o atender sin pago, de manera exclusiva  a  niños, ancianos,  enfermos  o discapacitados?  (bañarlos, cambiarlos, )
3realizar compras, llevar cuentas o realizar trámites para el hogar o encargarse de la seguridad?(como guardar el automóvil)
4llevar a algún miembro del hogar a la escuela, cita médica u otra actividad?
5construir o ampliar su vivienda?
6reparar o dar mantenimiento a su vivienda, muebles, aparatos electrodomésticos o vehículos?
7realizar los quehaceres de su hogar? (lavar, planchar, preparar y servir alimentos, barrer)
8prestar  servicios  gratuitos a su comunidad?(conseguir despensas, cuidar personas en un hospital)
*/



gen t_estudiar=p11_h1*60+p11_m1 if p11_h1<98

gen t_cuidado=p11_h2*60+p11_m2 if p11_h2<98

gen t_construir=p11_h3*60+p11_m3 if p11_h3<98 & year<2013
replace t_construir=p11_h5*60+p11_m5 if p11_h5<98 & year>2012

gen t_reparar=p11_h4*60+p11_m4 if p11_h4<98 & year<2013
replace t_reparar=p11_h6*60+p11_m6 if p11_h6<98 & year>2012

gen t_quehacer=p11_h5*60+p11_m5 if p11_h5<98 & year<2013
replace t_quehacer=p11_h7*60+p11_m7 if p11_h7<98 & year>2012

gen t_comun=p11_h6*60+p11_m6 if p11_h6<98 & year<2013
replace t_comun=p11_h8*60+p11_m8 if p11_h8<98 & year>2012

gen t_compras=p11_h4*60+p11_m4 if p11_h4<98 & year>2012

gen t_traslado=p11_h5*60+p11_m5 if p11_h5<98 & year>2012

egen t_total=rowtotal(t_cuidad-t_traslado)
egen t_total0=rowtotal(t_cuidad-t_comun)

gen t_total_hrs=t_total/60



foreach var of varlist t_estudiar-t_traslado {
replace `var'=`var'/60
replace `var'=0 if `var'==.

}

**** Recode

gen ocup_old=0 if eda>14
replace ocup_old=1 if p1coe==1 & eda>14 /* ocupados plenos*/
replace ocup_old=1  if p1a1==1 & eda>14 /*Ocupados sin pago*/
replace ocup_old=1  if p1a2==2 & eda>14 /*Ocupados sin pago*/
replace ocup_old=1  if p1c<5 & eda>14  /*Ocupados ausente con nexo laboral*/
replace ocup_old=1  if p1d==1 & eda>14  /*Ocupados ausente con retorno*/ 
replace ocup_old=1  if p1d==2 & p1e==1 & eda>14  /*Ocupados ausente con retorno*/
replace ocup_old=1  if p1d==9 & p1e==1 & eda>14 /*Ocupados ausente con retorno*/


gen dsmpleo_old=0 if eda>14
replace dsmpleo_old=1 if p1c==11 & eda>14 /* Iniciadores*/
replace dsmpleo_old=1 if p1b==2 & (p2_1==1 | p2_2==2 | p2_3==3) & p2b==1 & (p2c!=2 & p2c!=9) & eda>14 /* Desocupados con bÃÂsqueda*/
replace dsmpleo_old=1 if p2b==1 & (p2_1==1 | p2_2==2 | p2_3==3) & (p2c!=2 & p2c!=9) & (p1d==2 | p1d==9) & eda>14 /* Ausentes sin ingreso ni nexo laboral*/

gen pea_old=0
replace pea_old=1 if ocup_old==1
replace pea_old=2 if dsmpleo_old==1
gen pet=1 if eda>14

gen pea2=((clase2==1 | pea_old==2) & pea_old!=0) if pet==1

**Codificaciónn variables

recode eda (98/99=.)
egen edad5=cut(eda), at(10(5)100)
replace edad5=65 if eda>65
replace edad5=12 if edad5==10

egen edad10=cut(eda), at(10(10)100)
replace edad10=60 if eda>60
replace edad10=12 if edad10==10


** asiste a la escuela
gen asiste=cs_p17==1
replace asiste=. if cs_p17==.
replace asiste=. if cs_p17==9
label define asiste 1 "Asiste escuela" 0 "No asiste", modify
label values asiste

*** Estado conyugal

gen unido=(e_con==1 | e_con==5) if e_con!=.
label define unido 1 "Unido" 0 "No unido", modify
label values unido unido

recode anios_esc (99=.)

recode par_c (101=1) (201/205=2) (301/304=3) (else=4), gen(parentesco) 
gen par_dic=(par_c==101)
lab var parentesco "Parentesco"
lab define parentesco 1 "Jefe(a)" 2 "Cónyuge" 3 "Hijo(a)" 4 "Otro(a)"
lab value parentesco parentesco

recode t_loc (1/3=1 "Urban") (4=2 "Rural"), gen(urb_rur)
gen rural=urb_rur==2

*** parentesco

gen relative=. 
replace relative=1 if (par_c>=100 & par_c<200)
replace relative=2 if (par_c>=200 & par_c<300)
replace relative=3 if (par_c>=300 & par_c<400)
replace relative=4 if (par_c>=400 & par_c<403)
replace relative=5 if (par_c>=404 & par_c <424)
recode relative(.=6) if  relative==.
replace relative=7 if par_c==601
replace relative=7 if par_c==612


*** Tamaño del hogar

egen tam_hog=total(relative!=7), by(folio2)
label var tam_hog "Tamaño del hogar"

*** Tasas de dependencia por hogar

egen men1=total(eda<15), by(folio2)
egen men2=total(eda==99), by(folio2)
egen may=total(eda>64 & eda!=99), by (folio2)
egen men=rowtotal(men1 men2)
egen dep=rowtotal(men may)
egen nondep=total(eda>15 & eda<65), by (folio2)

gen t_dep1= men/nondep
label var t_dep1 "Tasa dependencia menores"
gen t_dep2= may/nondep
label var t_dep2 "Tasa dependencia mayores"
gen t_dep3= dep/nondep
label var t_dep3 "Tasa dependencia total"

*** Tasa de ocupación del hogar 
egen ocup=total(clase2==1), by(folio2)
gen t_act=ocup/tam_hog
label var t_act "Tasa ocupación hogar"

**** Presencia menores de 6 años en el hogar

egen men6=total(eda<6), by(folio2)
gen  men6d=(men6>0)
label var men6d "Presencia de menores de 6 años"
label define men6d  0 "No menores<6" 1 "Sí menores<6", modify
label values men6d men6d


*** Variables para hacer merge
gen str4 sinco4d=string(p3coe)
gen str3 sinco3d = substr(sinco4d, 1,3)
destring  sinco3d, replace


gen str4 st_sinco4d=string(p7a)
gen str3 st_sinco3d = substr(sinco4d, 1,3)
destring  st_sinco3d, replace


gen multi=p7<7 if clase2==1
label var multi "Multiactividad"


*** imputando valores, 0 a veces no es un valor válido
clonevar ing_x_hrs0=ing_x_hrs

gen miss_income=p6b1>6 // 1 es missing
replace ing_x_hrs=. if miss_income==1 & ing_x_hrs==0

gen sm_hr=salario/hrsocup

replace ing_x_hrs = 0.5*sm_hr if p6c==1 & miss_income==1 & ing_x_hrs0==0
replace ing_x_hrs =     sm_hr if p6c==2 & miss_income==1 & ing_x_hrs0==0
replace ing_x_hrs = 1.5*sm_hr if p6c==3 & miss_income==1 & ing_x_hrs0==0
replace ing_x_hrs = 2.5*sm_hr if p6c==4 & miss_income==1 & ing_x_hrs0==0
replace ing_x_hrs = 4.0*sm_hr if p6c==5 & miss_income==1 & ing_x_hrs0==0
replace ing_x_hrs = 7.5*sm_hr if p6c==6 & miss_income==1 & ing_x_hrs0==0
replace ing_x_hrs =10.5*sm_hr if p6c==7 & miss_income==1 & ing_x_hrs0==0
replace ing_x_hrs=0 if ing7c==6 & ing_x_hrs0==0

gen miss_income2=ing_x_hrs==.

save "$cleandat/enoet1_2020_vars.dta", replace

log close


* -----------------------------------------------------------------------------
*  CLASIFICACIÓN - PARA IMPORTAR A STATA
* -----------------------------------------------------------------------------

log using "$log/covid19.log", replace

** importando homologación

import excel $class/class_14may.xlsx, sheet("sinco4d") firstrow clear // archivo que se entrega
save "$class/sinco.dta", replace
renvars *, prefix(st_)
rename st_p3coe st_sinco4d
save "$class/st_sinco.dta", replace


import excel $class/class_14may.xlsx, sheet("scian4d") firstrow clear // archivo que se entrega
save "$class/scian.dta", replace
renvars *, prefix(st_)
rename st_p4a p7c
save "$class/st_scian.dta", replace


use  "$cleandat/enoet1_2020_vars.dta", clear


* -----------------------------------------------------------------------------
*  ACTIVIDADES///OCUPACIONES ESENCIALES
* -----------------------------------------------------------------------------


**** TRABAJJO PRINCIPAL
merge n:1 p3coe using $class/sinco.dta, gen(m1)
drop if m1==2

merge n:1 p4a using $class/scian.dta, gen(m2)
drop if m2==2
drop m1 m2


**** TRABAJO SECUNDARIO

destring st_sinco4d, replace

merge n:1 st_sinco4d using $class/st_sinco.dta, gen(m3)
drop if m3==2

merge n:1 p7c using $class/st_scian.dta, gen(m4)
drop if m4==2

drop m3 m4



**** Sustituyendo sector gub en actividades
 
replace class_scian1=3 if gob_ss==. & class_scian1==5 // Queda en frontera
replace class_scian1=1 if gob_ss==1 & class_scian1==5  // Queda en esencial

replace st_class_scian1=3 if st_gob_ss==. & st_class_scian1==5 // Queda en frontera
replace st_class_scian1=1 if st_gob_ss==1 & st_class_scian1==5  // Queda en esencial



**** Imputando riesgos más altos para hospitales

/* Ramas hospitales
6221
6222
6229*/

gen hospital=p4a==6221
replace hospital=1 if p4a==6222
replace hospital=1 if p4a==6229

gen st_hospital=p7c==6221
replace st_hospital=1 if p7c==6222
replace st_hospital=1 if p7c==6229


*** SINCO


*** riesgo según rama
replace sinco_riesgo=sinco_riesgo+obs if hospital==1 & obs>0 & obs!=.
replace st_sinco_riesgo=st_sinco_riesgo+st_obs if st_hospital==1 & st_obs>0 & st_obs!=.

*** Profesores  AHORITA SON BAJO.

replace sinco_riesgo=sinco_riesgo+obs if hospital==1 & obs==-1 & obs!=.
replace st_sinco_riesgo=st_sinco_riesgo+st_obs if st_hospital==1 & st_obs==-1 & st_obs!=.

*** OCUPACIONES QUE SON ESENCIALES DEPENDIENDO DE LA RAMA

replace sinco_esencial=1 if dof_obs==1 & class_scian1<3
replace sinco_esencial=0 if dof_obs==1 & class_scian1>2

replace st_sinco_esencial=1 if st_dof_obs==1 & class_scian1<3
replace st_sinco_esencial=0 if st_dof_obs==1 & class_scian1>2

*** OCUPACIONES QUE SON ESENCIALES QUE NO DEPENDEN DE LA RAMA

*** Vigilancia y otras // Aunque el negocio esté cerrado siguen vigilando

replace class_scian1=1 if esencial_para_todos==1
replace st_class_scian1=1 if st_esencial_para_todos==1


**** OCUPACIONES QUE POR SU DESCRIPCIÓN PODRÍAN SER ESENCIALES 
***** PERO EN ACTIVIDADES NO ESENCIALES, QUEDAN EN FRONTERA

replace class_scian1=3 if class_scian1==4 & sinco_esencial==1
replace st_class_scian1=3 if st_class_scian1==4 & st_sinco_esencial==1


*** Actividades esenciales
label var class_scian1 "Actividades esenciales"
label define class_scian1 1 "Esenciales DOF" 2 "Encadenamiento" ///
3 "Frontera - mixta" 4 "No esenciales" ///
5 "Cruce", modify


label values class_scian1 class_scian1
label values st_class_scian1 class_scian1

tab class_scian1 if clase2==1 [fw=fac]

label var sinco_riesgo "Riesgo ocupaciones"
label define sinco_riesgo 1 "Bajo" 2 "Medio" 3 "Alto" 4 "Muy alto", modify

label values sinco_riesgo sinco_riesgo
label values st_sinco_riesgo st_sinco_riesgo

tab sinco_riesgo if clase2==1 [fw=fac]


tab class_scian1 sinco_riesgo if clase2==1 [fw=fac], row col cell

tab class_scian1 sinco_esencial if clase2==1 [fw=fac], row col cell

gen seq_class_scian=class_scian1 if clase2==1
replace seq_class=5 if clase2>1

label var seq_class_scian "Actividades esenciales"
label define seq_class_scian 1 "Esenciales DOF" 2 "Encadenamiento" ///
3 "Frontera - mixta" 4 "No esenciales" ///
5 "Fuera", modify

label values seq_class_scian seq_class_scian

save "$cleandat/enoet1_2020_covid.dta", replace


log close
