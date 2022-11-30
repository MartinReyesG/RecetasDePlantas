
%retama=esparte�na
%rosal=vitamina C, qurcitrina

%�Cuales son plantas o plantas medicinales?
%*******************************************************************************************
planta(retema).
planta(ricino).
planta(rosal).
planta(romero).
planta(ruda).
planta(simonillo).
planta(tamarindo).
planta(tabach�n).
planta(taray).
planta(tila).


%Listar plantas medicinales y su nombre cient�fico
%*******************************************************************************************
nombreCientifico(retema,'spartium junceum').
nombreCientifico(ricino,'ricinus communis').
nombreCientifico(rosal,'rosa centifolia').
nombreCientifico(romero,'salvia rosmarinus').
nombreCientifico(ruda,'ruta graveolens').
nombreCientifico(simonillo,'laennecia schiedeana').
nombreCientifico(tamarindo,'tamarindus indica').
nombreCientifico(tabach�n,'cesalpinea pulcherrima').
nombreCientifico(taray,'amarix gallica').
nombreCientifico(tila,'tilia europea').

%Explicaci�n de los modos de preparaci�n
%*******************************************************************************************
preparacionDeHiervas(cocimiento,'Hervir agua, poner la planta, hervir 10 min, reposar 5 min').
preparacionDeHiervas(infusi�n, 'Hervir agua, colocar la planta, tapar y reposar 5 min').
preparacionDeHiervas(maceraci�n,'Triturar la planta, poner en agua y dejar remojar').
preparacionDeHiervas(jarabe,'hervir 10 min, agregar azucar, agregar 10% de alcohol de ca�a y envasar').
preparacionDeHiervas(tintura,'En envase con corcho poner la planta triturada y se agrega alcohol de ca�a y agua destilada, reposar en oscuridad una o dos semanas').
preparacionDeHiervas(jugo,'exprimir la planta en un trapo limpio').
preparacionDeHiervas(horchata,'se muelen las semillas agregando agua para formar una masilla, se cuela, se endulza y agrega agua para tomar').



%�Cuales son las  enfermedades que curan las plantas?.
%*******************************************************************************************
elimina(retema,par�titos).
elimina(retema,hidropes�a).
elimina(retema,acumulaci�n_de_toxinas).
elimina(retema,problemas_renales).

elimina(ricino,estre�imiento).
elimina(ricino,peritonitis).
elimina(ricino,lombrices).
elimina(ricino,hernia_estrangulada).
elimina(ricino,par�titos).
elimina(ricino,ca�da_de_cabello).
elimina(ricino,caspa).
elimina(ricino,seborrea).

elimina(rosal,estre�imiento).
elimina(rosal,c�licos_infantles).
elimina(rosal,empachos).
elimina(rosal,conjuntivitis).

elimina(romero,infecciones_vaginales).
elimina(romero,baja_energ�a).
elimina(romero,problemas_mestruales).
elimina(romero,mala_digesti�n).
elimina(romero,estr�s).
elimina(romero,ca�da_de_cabello).

elimina(ruda,partos_dif�ciles).
elimina(ruda,hijos_no_deseados).
elimina(ruda,mal_humor_en_ciclo_menstrual).
elimina(ruda,jaquecas).
elimina(ruda,problemas_mestruales).

elimina(simonillo,estre�imiento).
elimina(simonillo,ictericia).
elimina(simonillo,catarro_v�a_biliares).
elimina(simonillo,c�lico_hep�tico).
elimina(simonillo,dispepsia_cr�nica).

elimina(tamarindo,problemas_renales).
elimina(tamarindo,estre�imiento).
elimina(tamarindo,sed).
elimina(tamarindo,empachos).
elimina(tamarindo,indigesti�n_infantil).

elimina(tabach�n,h�gado_intoxicado).
elimina(tabach�n,ri�on_intoxicado).
elimina(tabach�n,hijos_no_deseados).
elimina(tabach�n,problemas_mestruales).

elimina(taray,problemas_renales).
elimina(taray,c�lculos_renales).

elimina(tila,nerviosismo).
elimina(tila,malestar_general).
elimina(tila,problemas_de_sue�o).
elimina(tila,dolor_hist�rico_de_cabeza).
elimina(tila,fiebre).
elimina(tila,tos_espasm�dica).
elimina(tila,estr�s).


%�Cuales son los modos de preparaci�n de una planta en especifico?
%*******************************************************************************************
preparacionDePlanta(retema,'las ramas, flores y semillas se hierven').
preparacionDePlanta(retema,'se hierve un mu�ado de cenizas de la planta en un litro de agua y se usa como t�').
preparacionDePlanta(ricino,'t�').
preparacionDePlanta(ricino,'se mezclan 40 gr de aceite de ricino y 10 gr de jugo de ortigas').
preparacionDePlanta(rosal,'t�').
preparacionDePlanta(romero,'t�').
preparacionDePlanta(romero,'cigarillo').
preparacionDePlanta(ruda,'t�').
preparacionDePlanta(simonillo,'se hirve toda la planta y se usa como t�').
preparacionDePlanta(tamarindo,'jugo').
preparacionDePlanta(tabach�n,'t�').
preparacionDePlanta(taray,'se remoja la madera toda la noche y se usa como agua de uso').
preparacionDePlanta(tila,'t�').


%Modos de preparaci�n de cada planta
%*******************************************************************************************
uso(retema,cocimiento).
uso(ricino,infusi�n).
uso(rosal,infusi�n).
uso(romero,cocimiento).
uso(ruda,infusi�n).
uso(simonillo,cocimiento).
uso(tamarindo,jugo).
uso(tabach�n,infusi�n).
uso(taray,maceraci�n).
uso(tila,cocimiento).


%Glosario de efectos de las plantas
%*******************************************************************************************
efectoPlanta(abortiva,'evita la formaci�n del feto').
efectoPlanta(afrodis�aca,'exita el apetito sexual').
efectoPlanta(analg�sica,'quita o modera el dolor').
efectoPlanta(anest�sica,'insensibiliza el cuerpo').
efectoPlanta(antidiarreica,'controla diarreas').
efectoPlanta(antiespasm�dica,'controla espasmos nerviosos').
efectoPlanta(antiflog�stica,'actua contra inflamaciones').
efectoPlanta(antipir�tica,'quita la fiebre').
efectoPlanta(antis�ptica,'mata los tejidos').
efectoPlanta(aperitiva,'produce apetito').
efectoPlanta(astringente,'hace contraer los tejidos').
efectoPlanta(carminativa,'evita la formaci�n de gases o provoca su expulsion').
efectoPlanta(colagoga,'ayuda a expulsar la bilis').
efectoPlanta(depurativa,'limpia y purifica la sangre').
efectoPlanta(diafor�tica,'provoca sudar').
efectoPlanta(digestiva,'favorece la digestion').
efectoPlanta(diur�tica,'provoca la orina').
efectoPlanta(em�tica,'provoca nauseas y vomitos').
efectoPlanta(emenagoga,'activa la menstruacion').
efectoPlanta(estupefciente,'aquieta, duerme o atonta').
efectoPlanta(estimulante,'aumenta el estado de energ�a').
efectoPlanta(expectorante,'expulsa las flemas').
efectoPlanta(hemost�tica,'corta o detiene hemorragias').
efectoPlanta(hep�tica,'ayuda al higado').
efectoPlanta(laxante,'purga sin provocar diarrea').
efectoPlanta(pectoral,'ayuda al pecho').
efectoPlanta(refrescante,'alivia la sed').
efectoPlanta(sedante,'calma dolores intestinales').
efectoPlanta(t�nica,'da fuerza al organismo').
efectoPlanta(t�xica,'es venenosa').
efectoPlanta(verm�fuga,'expulsa gusanos intestinales').
efectoPlanta(vulneraria,'cura llagas y heridas').


%*******************************************************************************************
%�Cuales son las acciones o efectos que tienen las plantas?    
efecto(retema,laxante).
efecto(retema,diur�tica).
efecto(retema,depurativa).

efecto(ricino,digestiva).
efecto(ricino,laxante).
efecto(ricino,astringente).
efecto(ricino,verm�fuga).
efecto(ricino,t�xica).

efecto(rosal,laxante).
efecto(rosal,astringente).
efecto(rosal,digestiva).


efecto(romero,estimulante).
efecto(romero,emenagoga).
efecto(romero,digestiva).


efecto(ruda,emenagoga).
efecto(ruda,abortiva).
efecto(ruda,antiespasm�dica).
efecto(ruda,sedante).


efecto(simonillo,digestiva).
efecto(simonillo,colagoga).
efecto(simonillo,hep�tica).


efecto(tamarindo,laxante).
efecto(tamarindo,diur�tica).
efecto(tamarindo,refrescante).
efecto(tamarindo,digestiva).


efecto(tabach�n,depurativa).
efecto(tabach�n,abortiva).
efecto(tabach�n,emenagoga).
efecto(tabach�n,colagoga).

efecto(taray,diur�tica).

efecto(tila,sedante).
efecto(tila,analg�sica).
efecto(tila,antipir�tica).
efecto(tila,antiespasm�dica).


%�Qu� elementos tiene una planta en especifica?
%*******************************************************************************************

parteDePlanta(retema, 'ramas, flores, semillas').
parteDePlanta(ricino, 'semillas').
parteDePlanta(rosal, 'petalos, pistilos, caliz').
parteDePlanta(romero, 'hojas, flores, aceites esenciales').
parteDePlanta(ruda, 'hojas').
parteDePlanta(simonillo, 'desconocido').
parteDePlanta(tamarindo, 'hojas, flor, fruto, tronco, ramas, ra�z.').
parteDePlanta(tabach�n, 'hojas, flores, ramas, fruto').
parteDePlanta(taray, 'flores, ramas').
parteDePlanta(tila, 'flores').

%�Cual es el origen de una planta?
%******************************************************************************************



%*******************************************************************************************

aplicacion(par�titos,interno,t�).
aplicacion(hidropes�a,interno,t�).
aplicacion(acumulaci�n_de_toxinas,interno,t�).
aplicacion(problemas_renales,interno,t�).
aplicacion(estre�imiento,interno,t�).
aplicacion(peritonitis,interno,t�).
aplicacion(lombrices,interno,t�).
aplicacion(hernia_estrangulada,interno,t�).
aplicacion(ca�da_de_cabello,externo,aceite).
aplicacion(caspa,externo,aceite).
aplicacion(seborrea,externo,aceite).
aplicacion(c�licos_infantles,interno,infusi�n).
aplicacion(empachos,interno,infusi�n).
aplicacion(conjuntivitis,externo,infusi�n).
aplicacion(infecciones_vaginales,externo,t�).
aplicacion(baja_energ�a,interno,t�).
aplicacion(problemas_mestruales,interno,t�).
aplicacion(mala_digesti�n,interno,t�).
aplicacion(estr�s,interno,t�).
aplicacion(partos_dif�ciles,interno,t�).
aplicacion(hijos_no_deseados,interno,t�).
aplicacion(mal_humor_en_ciclo_menstrual,interno,t�).
aplicacion(jaquecas,interno,t�).
aplicacion(ictericia,interno,t�).
aplicacion(catarro_v�a_biliares,interno,t�).
aplicacion(c�lico_hep�tico,interno,t�).
aplicacion(dispepsia_cr�nica,interno,t�).
aplicacion(sed,interno,jugo).
aplicacion(indigesti�n_infantil,interno,agua).
aplicacion(h�gado_intoxicado,interno,t�).
aplicacion(ri�on_intoxicado,interno,t�).
aplicacion(c�lculos_renales,interno,remojo).
aplicacion(nerviosismo,interno,t�).
aplicacion(malestar_general,interno,t�).
aplicacion(problemas_de_sue�o,interno,t�).
aplicacion(dolor_hist�rico_de_cabeza,interno,t�).
aplicacion(fiebre,interno,t�).
aplicacion(tos_espasm�dica,interno,t�).


%REGLAS-------------------------------------------------------------------

recetar(AFECCIONES,PLANTA,MODO,PREPARACION):-elimina(PLANTA,AFECCIONES),aplicacion(AFECCIONES,MODO,PREPARACION).
%recetar(X,Y,interno,t�).
%recetar(estre�imiento,Y,interno,t�).
%recetar(partos_dif�ciles,X,Z,R).
%recetar(X,tila,Z,R).




usar(AFECCION,PREPARACION):-modoDePreparacion(PREPARACION,AFECCION).
%usar(estr�s,X).

buscarNombreCient�fico(PLANTA,CIENTIFICO):-nombreCientifico(PLANTA,CIENTIFICO).
%buscarNombreCient�fico(romero,Y).

t�(X):-modoDePreparacion(t�,X).
aceite(X):-modoDePreparacion(aceite,X).
agua(X):-modoDePreparacion(agua,X).
infusi�n(X):-modoDePreparacion(infusi�n,X).
remojo(X):-modoDePreparacion(remojo,X).

ingerir(X,Z,Y):-elimina(X,Z),modoDeAplicacion(Z,Y).
%ingerir(ruda,X,Y).



%funciones extras sin probar---------------------------------------------------------------------------------------
resumen_planta(X,Y,Z):-nombreCientifico(X,Y),elimina(X,Z).
modo_uso(X,Z,W,Y):-uso(X,Y),preparacionDeHiervas(Y,Z),preparacionDePlanta(X,W).
efectoNombreplanta(Y,X):-efecto(X,Y),efectoPlanta(Y,_).
efectoXPlanta(Z,X):-efecto(X,Y),efectoPlanta(Y,Z).


%*******************************************************************************************
%�Cuales son las plantas que curan el herpes?
plantasCuranHerpes(Planta):-elimina(Planta,herpes).

%�Qu� plantas son analg�sicas?
plantasAnalgesica(Planta):-efecto(Planta,analg�sica).

