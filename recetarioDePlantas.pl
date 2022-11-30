%Lista de plantas
%*******************************************************************************************
planta(retema).
planta(ricino).
planta(rosal).
planta(romero).
planta(ruda).
planta(simonillo).
planta(tamarindo).
planta(tabachín).
planta(taray).
planta(tila).

%Contenido de plantas
%*******************************************************************************************
contenidoDeLasPlantas(planta, vitaminas).
contenidoDeLasPlantas(planta, hormonas).
contenidoDeLasPlantas(planta, minerales).
contenidoDeLasPlantas(planta, metaloides).
contenidoDeLasPlantas(planta, proteínas).
contenidoDeLasPlantas(planta, enzimas).
contenidoDeLasPlantas(planta, alcaloides).

%Efectos secundarios de los medicamentos
%*******************************************************************************************
efectosSecundariosMedicamentos(medicamento, 'congestion hepática').
efectosSecundariosMedicamentos(medicamento, 'fatiga excesiva').
efectosSecundariosMedicamentos(medicamento, adicción).
efectosSecundariosMedicamentos(medicamento, alergias).
efectosSecundariosMedicamentos(medicamento, 'eliminación de la flora intestinal').
efectosSecundariosMedicamentos(medicamento, gastritis).
efectosSecundariosMedicamentos(medicamento, colitis).

%Listado de plantas que producen medicamentos
%*******************************************************************************************
plantasProdMedic(digital,digitalina).
plantasProdMedic(digital,'tónico cardiaco').
plantasProdMedic(opio,morfina).
plantasProdMedic(opio,codeina).
plantasProdMedic(ipeca,emetina).
plantasProdMedic('nuez vómica',estricnina).
plantasProdMedic('eléboro blanco',veratrina).
plantasProdMedic(colchico,colquicina).
plantasProdMedic(belladona,atronina).
plantasProdMedic(quina,quinina).
plantasProdMedic(cacao,teobromina).
plantasProdMedic(retama,esparteína).
plantasProdMedic(coca,cacaína).
plantasProdMedic(peyote,mescalina).
plantasProdMedic(efedra,efedrina).
plantasProdMedic(barrasco,hormonas).
plantasProdMedic('nenúfar amarillo',lutenurina).
plantasProdMedic(ñame,diosponina).
plantasProdMedic(artemisa,tauremisina).
plantasProdMedic('Semilla de yute',olitorisida).
plantasProdMedic(toloache,'ácido lisergico (LSD)').
plantasProdMedic(eucalipto,eucaliptol).
plantasProdMedic(rosal,'vitamina C').
plantasProdMedic(rosal,quercitrina).



%Listado de plantas esenciales
%*******************************************************************************************
botiquin('anís estrella').
botiquin(menta).
botiquin(árnica).
botiquin(salvia).
botiquin(tila).
botiquin(eucalipto).
botiquin(yerbabuena).
botiquin(manzanilla).
botiquin('cola de caballo').
botiquin(romero).
botiquin(toronjil).
botiquin(sanguinaria).
botiquin(linaza).
botiquin(hamamelis).
botiquin(zarzaparrilla).
botiquin(boldo).
botiquin('diente de león').
botiquin(azahar).
botiquin(malva).
botiquin(marrubio).
botiquin(rosal).

%Listar plantas medicinales y su nombre científico
%*******************************************************************************************
nombreCientifico(retema,'spartium junceum').
nombreCientifico(ricino,'ricinus communis').
nombreCientifico(rosal,'rosa centifolia').
nombreCientifico(romero,'salvia rosmarinus').
nombreCientifico(ruda,'ruta graveolens').
nombreCientifico(simonillo,'laennecia schiedeana').
nombreCientifico(tamarindo,'tamarindus indica').
nombreCientifico(tabachín,'cesalpinea pulcherrima').
nombreCientifico(taray,'amarix gallica').
nombreCientifico(tila,'tilia europea').

%Explicación de los modos de preparación
%*******************************************************************************************
preparacionDeHiervas(cocimiento,'Hervir agua, poner la planta, hervir 10 min, reposar 5 min').
preparacionDeHiervas(infusión, 'Hervir agua, colocar la planta, tapar y reposar 5 min').
preparacionDeHiervas(maceración,'Triturar la planta, poner en agua y dejar remojar').
preparacionDeHiervas(jarabe,'hervir 10 min, agregar azucar, agregar 10% de alcohol de caña y envasar').
preparacionDeHiervas(tintura,'En envase con corcho poner la planta triturada y se agrega alcohol de caña y agua destilada, reposar en oscuridad una o dos semanas').
preparacionDeHiervas(jugo,'exprimir la planta en un trapo limpio').
preparacionDeHiervas(horchata,'se muelen las semillas agregando agua para formar una masilla, se cuela, se endulza y agrega agua para tomar').



%Lista de las plantas y que enfermedades alivia
%*******************************************************************************************
elimina(retema,parátitos).
elimina(retema,hidropesía).
elimina(retema,'acumulación de toxinas').
elimina(retema,'problemas renales').

elimina(ricino,estreñimiento).
elimina(ricino,peritonitis).
elimina(ricino,lombrices).
elimina(ricino,'hernia estrangulada').
elimina(ricino,parátitos).
elimina(ricino,'caída de cabello').
elimina(ricino,caspa).
elimina(ricino,seborrea).

elimina(rosal,estreñimiento).
elimina(rosal,'cólicos infantles').
elimina(rosal,empachos).
elimina(rosal,conjuntivitis).

elimina(romero,'infecciones vaginales').
elimina(romero,'baja energía').
elimina(romero,'problemas mestruales').
elimina(romero,'mala digestión').
elimina(romero,estrés).
elimina(romero,'caída de cabello').

elimina(ruda,'partos difíciles').
elimina(ruda,'hijos no deseados').
elimina(ruda,'mal humor en ciclo menstrual').
elimina(ruda,jaquecas).
elimina(ruda,'problemas mestruales').

elimina(simonillo,estreñimiento).
elimina(simonillo,ictericia).
elimina(simonillo,'catarro de vía biliares').
elimina(simonillo,'cólico hepático').
elimina(simonillo,'dispepsia crónica').

elimina(tamarindo,'problemas renales').
elimina(tamarindo,estreñimiento).
elimina(tamarindo,sed).
elimina(tamarindo,empachos).
elimina(tamarindo,'indigestión infantil').

elimina(tabachín,'hígado intoxicado').
elimina(tabachín,'riñon intoxicado').
elimina(tabachín,'hijos no deseados').
elimina(tabachín,'problemas mestruales').

elimina(taray,'problemas renales').
elimina(taray,'cálculos renales').

elimina(tila,nerviosismo).
elimina(tila,'malestar general').
elimina(tila,'problemas de sueño').
elimina(tila,'dolor histérico de cabeza').
elimina(tila,fiebre).
elimina(tila,'tos espasmódica').
elimina(tila,estrés).


%Método de preparación de las plantas
%*******************************************************************************************
preparacionDePlanta(retema,'las ramas, flores y semillas se hierven').
preparacionDePlanta(retema,'se hierve un muñado de cenizas de la planta en un litro de agua y se usa como té').
preparacionDePlanta(ricino,'té').
preparacionDePlanta(ricino,'se mezclan 40 gr de aceite de ricino y 10 gr de jugo de ortigas').
preparacionDePlanta(rosal,'té').
preparacionDePlanta(romero,'té').
preparacionDePlanta(romero,'cigarillo').
preparacionDePlanta(ruda,'té').
preparacionDePlanta(simonillo,'se hirve toda la planta y se usa como té').
preparacionDePlanta(tamarindo,'jugo').
preparacionDePlanta(tabachín,'té').
preparacionDePlanta(taray,'se remoja la madera toda la noche y se usa como agua de uso').
preparacionDePlanta(tila,'té').


%Modos de preparación de cada planta
%*******************************************************************************************
uso(retema,cocimiento).
uso(ricino,infusión).
uso(rosal,infusión).
uso(romero,cocimiento).
uso(ruda,infusión).
uso(simonillo,cocimiento).
uso(tamarindo,jugo).
uso(tabachín,infusión).
uso(taray,maceración).
uso(tila,cocimiento).


%Glosario de efectos de las plantas
%*******************************************************************************************
efectoPlanta(abortiva,'evita la formación del feto').
efectoPlanta(afrodisíaca,'exita el apetito sexual').
efectoPlanta(analgésica,'quita o modera el dolor').
efectoPlanta(anestésica,'insensibiliza el cuerpo').
efectoPlanta(antidiarreica,'controla diarreas').
efectoPlanta(antiespasmódica,'controla espasmos nerviosos').
efectoPlanta(antiflogística,'actua contra inflamaciones').
efectoPlanta(antipirética,'quita la fiebre').
efectoPlanta(antiséptica,'mata los tejidos').
efectoPlanta(aperitiva,'produce apetito').
efectoPlanta(astringente,'hace contraer los tejidos').
efectoPlanta(carminativa,'evita la formación de gases o provoca su expulsion').
efectoPlanta(colagoga,'ayuda a expulsar la bilis').
efectoPlanta(depurativa,'limpia y purifica la sangre').
efectoPlanta(diaforética,'provoca sudar').
efectoPlanta(digestiva,'favorece la digestion').
efectoPlanta(diurética,'provoca la orina').
efectoPlanta(emética,'provoca nauseas y vomitos').
efectoPlanta(emenagoga,'activa la menstruacion').
efectoPlanta(estupefciente,'aquieta, duerme o atonta').
efectoPlanta(estimulante,'aumenta el estado de energía').
efectoPlanta(expectorante,'expulsa las flemas').
efectoPlanta(hemostática,'corta o detiene hemorragias').
efectoPlanta(hepática,'ayuda al higado').
efectoPlanta(laxante,'purga sin provocar diarrea').
efectoPlanta(pectoral,'ayuda al pecho').
efectoPlanta(refrescante,'alivia la sed').
efectoPlanta(sedante,'calma dolores intestinales').
efectoPlanta(tónica,'da fuerza al organismo').
efectoPlanta(tóxica,'es venenosa').
efectoPlanta(vermífuga,'expulsa gusanos intestinales').
efectoPlanta(vulneraria,'cura llagas y heridas').

%Lista de efectos de las plantas en el cuerpo
%*******************************************************************************************   
efecto(retema,laxante).
efecto(retema,diurética).
efecto(retema,depurativa).

efecto(ricino,digestiva).
efecto(ricino,laxante).
efecto(ricino,astringente).
efecto(ricino,vermífuga).
efecto(ricino,tóxica).

efecto(rosal,laxante).
efecto(rosal,astringente).
efecto(rosal,digestiva).


efecto(romero,estimulante).
efecto(romero,emenagoga).
efecto(romero,digestiva).


efecto(ruda,emenagoga).
efecto(ruda,abortiva).
efecto(ruda,antiespasmódica).
efecto(ruda,sedante).


efecto(simonillo,digestiva).
efecto(simonillo,colagoga).
efecto(simonillo,hepática).


efecto(tamarindo,laxante).
efecto(tamarindo,diurética).
efecto(tamarindo,refrescante).
efecto(tamarindo,digestiva).


efecto(tabachín,depurativa).
efecto(tabachín,abortiva).
efecto(tabachín,emenagoga).
efecto(tabachín,colagoga).

efecto(taray,diurética).

efecto(tila,sedante).
efecto(tila,analgésica).
efecto(tila,antipirética).
efecto(tila,antiespasmódica).


%Elementos de las plantas
%*******************************************************************************************

parteDePlanta(retema, 'ramas, flores, semillas').
parteDePlanta(ricino, 'semillas').
parteDePlanta(rosal, 'petalos, pistilos, caliz').
parteDePlanta(romero, 'hojas, flores, aceites esenciales').
parteDePlanta(ruda, 'hojas').
parteDePlanta(simonillo, 'desconocido').
parteDePlanta(tamarindo, 'hojas, flor, fruto, tronco, ramas, raíz.').
parteDePlanta(tabachín, 'hojas, flores, ramas, fruto').
parteDePlanta(taray, 'flores, ramas').
parteDePlanta(tila, 'flores').

%Origen de las plantas
%******************************************************************************************

origenPlanta(retema, 'noroeste de africa y la península ibérica').
origenPlanta(ricino, 'africa tropical').
origenPlanta(rosal, 'asia, persia y asia Menor.').
origenPlanta(romero, 'francia').
origenPlanta(ruda, 'europa').
origenPlanta(simonillo, 'Desconocido, pero se encuentra en mexico, colombia, peru, estados unidos, guatemala, ecuador.').
origenPlanta(tamarindo, 'india').
origenPlanta(tabachín, 'zonas templadas o calientes del pais').
origenPlanta(taray, 'Desconocido, pero se encuentra en mexico').
origenPlanta(tila, 'europa, se da tambien en los bosques mexicanos').

%*******************************************************************************************

aplicacion(parátitos,interno,té).
aplicacion(hidropesía,interno,té).
aplicacion(acumulación_de_toxinas,interno,té).
aplicacion(problemas_renales,interno,té).
aplicacion(estreñimiento,interno,té).
aplicacion(peritonitis,interno,té).
aplicacion(lombrices,interno,té).
aplicacion(hernia_estrangulada,interno,té).
aplicacion(caída_de_cabello,externo,aceite).
aplicacion(caspa,externo,aceite).
aplicacion(seborrea,externo,aceite).
aplicacion(cólicos_infantles,interno,infusión).
aplicacion(empachos,interno,infusión).
aplicacion(conjuntivitis,externo,infusión).
aplicacion(infecciones_vaginales,externo,té).
aplicacion(baja_energía,interno,té).
aplicacion(problemas_mestruales,interno,té).
aplicacion(mala_digestión,interno,té).
aplicacion(estrés,interno,té).
aplicacion(partos_difíciles,interno,té).
aplicacion(hijos_no_deseados,interno,té).
aplicacion(mal_humor_en_ciclo_menstrual,interno,té).
aplicacion(jaquecas,interno,té).
aplicacion(ictericia,interno,té).
aplicacion(catarro_vía_biliares,interno,té).
aplicacion(cólico_hepático,interno,té).
aplicacion(dispepsia_crónica,interno,té).
aplicacion(sed,interno,jugo).
aplicacion(indigestión_infantil,interno,agua).
aplicacion(hígado_intoxicado,interno,té).
aplicacion(riñon_intoxicado,interno,té).
aplicacion(cálculos_renales,interno,remojo).
aplicacion(nerviosismo,interno,té).
aplicacion(malestar_general,interno,té).
aplicacion(problemas_de_sueño,interno,té).
aplicacion(dolor_histérico_de_cabeza,interno,té).
aplicacion(fiebre,interno,té).
aplicacion(tos_espasmódica,interno,té).


%REGLAS-------------------------------------------------------------------

recetar(AFECCIONES,PLANTA,MODO,PREPARACION):-elimina(PLANTA,AFECCIONES),aplicacion(AFECCIONES,MODO,PREPARACION).
%recetar(X,Y,interno,té).
%recetar(estreñimiento,Y,interno,té).
%recetar(partos_difíciles,X,Z,R).
%recetar(X,tila,Z,R).




usar(AFECCION,PREPARACION):-modoDePreparacion(PREPARACION,AFECCION).
%usar(estrés,X).


%buscarNombreCientífico(romero,Y).

té(X):-modoDePreparacion(té,X).
aceite(X):-modoDePreparacion(aceite,X).
agua(X):-modoDePreparacion(agua,X).
infusión(X):-modoDePreparacion(infusión,X).
remojo(X):-modoDePreparacion(remojo,X).

ingerir(X,Z,Y):-elimina(X,Z),modoDeAplicacion(Z,Y).
%ingerir(ruda,X,Y).



%funciones extras sin probar---------------------------------------------------------------------------------------
resumen_planta(X,Y,Z):-nombreCientifico(X,Y),elimina(X,Z).
modo_uso(X,Z,W,Y):-uso(X,Y),preparacionDeHiervas(Y,Z),preparacionDePlanta(X,W).
efectoNombreplanta(Y,X):-efecto(X,Y),efectoPlanta(Y,_).
efectoXPlanta(Z,X):-efecto(X,Y),efectoPlanta(Y,Z).


%*******************************************************************************************

%¿Cuales son plantas o plantas medicinales?
listaDePlantas(X):-planta(X).

%¿Qué elementos se encuentran en las plantas?
elementosDeTodasLasPlanta(X,Y):-parteDePlanta(X,Y).

%¿Qué elementos tiene una planta en especifica? 
elementosDeCadaPlanta(Planta,X):-parteDePlanta(Planta,X).

%¿Qué plantas producen medicamentos?


%¿Qué medicamentos producen una planta en especifico?


%¿Qué medicamentos provienen de plantas?


%¿Cuales son las acciones o efectos de medicamentos provenientes de plantas?


%¿Cuales son los efectos o acciones de un medicamento en especifico?


%¿Cuales son las acciones o efectos que tienen las plantas?    


%Significado de palabras que son acciones o efectos de plantas sobre organismo


%Listado de plantas y sus acciones o efectos sobre el organismo


%¿Acciones o efectos de una planta en especifico


%¿Qué plantas son analgésicas?
plantasAnalgesica(Planta):-efecto(Planta,analgésica).

%Listar plantas medicinales y su nombre científico
buscarNombreCientífico(Planta,X):-nombreCientifico(Planta,X).

%¿Cuales son las  enfermedades que curan las plantas?.*************************************************
cura(X,Y):-planta(X), elimina(X,Y).

%¿Cuales son  las enfermedades que que cura una planta en específico? Zabila
curaDeCadaPlanta(Planta,Y):-planta(Planta), elimina(Planta,Y).

%¿Cuales son las plantas que curan el herpes?
plantasCuranHerpes(Planta):-elimina(Planta,herpes).

%¿Cuales son las formas de preparación para tratamiento de enfermedades con uso de plantas?


%¿Cuales son los modos de preparación de una planta en especifico?


%¿Cual es el tratamiento y su preparación para alguna enfermedad?


%¿Cuales son los origenes de las plantas medicinales?


%¿Cual es el origen de una planta?


%¿Cual es el tratamiento para una enfermedad (ya sea con plantas o medicamentos)?


%Botiquín de plantas
buscarEnBotiquin(Planta):-planta(Planta),botiquin(Planta).