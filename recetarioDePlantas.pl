
%retama=esparteína
%rosal=vitamina C, qurcitrina

%¿Cuales son plantas o plantas medicinales?
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



%¿Cuales son las  enfermedades que curan las plantas?.
%*******************************************************************************************
elimina(retema,parátitos).
elimina(retema,hidropesía).
elimina(retema,acumulación_de_toxinas).
elimina(retema,problemas_renales).

elimina(ricino,estreñimiento).
elimina(ricino,peritonitis).
elimina(ricino,lombrices).
elimina(ricino,hernia_estrangulada).
elimina(ricino,parátitos).
elimina(ricino,caída_de_cabello).
elimina(ricino,caspa).
elimina(ricino,seborrea).

elimina(rosal,estreñimiento).
elimina(rosal,cólicos_infantles).
elimina(rosal,empachos).
elimina(rosal,conjuntivitis).

elimina(romero,infecciones_vaginales).
elimina(romero,baja_energía).
elimina(romero,problemas_mestruales).
elimina(romero,mala_digestión).
elimina(romero,estrés).
elimina(romero,caída_de_cabello).

elimina(ruda,partos_difíciles).
elimina(ruda,hijos_no_deseados).
elimina(ruda,mal_humor_en_ciclo_menstrual).
elimina(ruda,jaquecas).
elimina(ruda,problemas_mestruales).

elimina(simonillo,estreñimiento).
elimina(simonillo,ictericia).
elimina(simonillo,catarro_vía_biliares).
elimina(simonillo,cólico_hepático).
elimina(simonillo,dispepsia_crónica).

elimina(tamarindo,problemas_renales).
elimina(tamarindo,estreñimiento).
elimina(tamarindo,sed).
elimina(tamarindo,empachos).
elimina(tamarindo,indigestión_infantil).

elimina(tabachín,hígado_intoxicado).
elimina(tabachín,riñon_intoxicado).
elimina(tabachín,hijos_no_deseados).
elimina(tabachín,problemas_mestruales).

elimina(taray,problemas_renales).
elimina(taray,cálculos_renales).

elimina(tila,nerviosismo).
elimina(tila,malestar_general).
elimina(tila,problemas_de_sueño).
elimina(tila,dolor_histérico_de_cabeza).
elimina(tila,fiebre).
elimina(tila,tos_espasmódica).
elimina(tila,estrés).


%¿Cuales son los modos de preparación de una planta en especifico?
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


%*******************************************************************************************
%¿Cuales son las acciones o efectos que tienen las plantas?    
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


%¿Qué elementos tiene una planta en especifica?
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

%¿Cual es el origen de una planta?
%******************************************************************************************



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

buscarNombreCientífico(PLANTA,CIENTIFICO):-nombreCientifico(PLANTA,CIENTIFICO).
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
%¿Cuales son las plantas que curan el herpes?
plantasCuranHerpes(Planta):-elimina(Planta,herpes).

%¿Qué plantas son analgésicas?
plantasAnalgesica(Planta):-efecto(Planta,analgésica).

