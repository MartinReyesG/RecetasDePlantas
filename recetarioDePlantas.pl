%Lista de plantas
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

%Contenido de plantas
%*******************************************************************************************
contenidoDeLasPlantas(planta, vitaminas).
contenidoDeLasPlantas(planta, hormonas).
contenidoDeLasPlantas(planta, minerales).
contenidoDeLasPlantas(planta, metaloides).
contenidoDeLasPlantas(planta, prote�nas).
contenidoDeLasPlantas(planta, enzimas).
contenidoDeLasPlantas(planta, alcaloides).

%Efectos secundarios de los medicamentos
%*******************************************************************************************
efectosSecundariosMedicamentos(medicamento, 'congestion hep�tica').
efectosSecundariosMedicamentos(medicamento, 'fatiga excesiva').
efectosSecundariosMedicamentos(medicamento, adicci�n).
efectosSecundariosMedicamentos(medicamento, alergias).
efectosSecundariosMedicamentos(medicamento, 'eliminaci�n de la flora intestinal').
efectosSecundariosMedicamentos(medicamento, gastritis).
efectosSecundariosMedicamentos(medicamento, colitis).

%Listado de plantas que producen medicamentos
%*******************************************************************************************
plantasProdMedic(digital,digitalina).
plantasProdMedic(digital,'t�nico cardiaco').
plantasProdMedic(opio,morfina).
plantasProdMedic(opio,codeina).
plantasProdMedic(ipeca,emetina).
plantasProdMedic('nuez v�mica',estricnina).
plantasProdMedic('el�boro blanco',veratrina).
plantasProdMedic(colchico,colquicina).
plantasProdMedic(belladona,atronina).
plantasProdMedic(quina,quinina).
plantasProdMedic(cacao,teobromina).
plantasProdMedic(retama,esparte�na).
plantasProdMedic(coca,caca�na).
plantasProdMedic(peyote,mescalina).
plantasProdMedic(efedra,efedrina).
plantasProdMedic(barrasco,hormonas).
plantasProdMedic('nen�far amarillo',lutenurina).
plantasProdMedic(�ame,diosponina).
plantasProdMedic(artemisa,tauremisina).
plantasProdMedic('Semilla de yute',olitorisida).
plantasProdMedic(toloache,'�cido lisergico (LSD)').
plantasProdMedic(eucalipto,eucaliptol).
plantasProdMedic(rosal,'vitamina C').
plantasProdMedic(rosal,quercitrina).



%Listado de plantas esenciales
%*******************************************************************************************
botiquin('an�s estrella').
botiquin(menta).
botiquin(�rnica).
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
botiquin('diente de le�n').
botiquin(azahar).
botiquin(malva).
botiquin(marrubio).
botiquin(rosal).

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



%Lista de las plantas y que enfermedades alivia
%*******************************************************************************************
elimina(retema,par�titos).
elimina(retema,hidropes�a).
elimina(retema,'acumulaci�n de toxinas').
elimina(retema,'problemas renales').

elimina(ricino,estre�imiento).
elimina(ricino,peritonitis).
elimina(ricino,lombrices).
elimina(ricino,'hernia estrangulada').
elimina(ricino,par�titos).
elimina(ricino,'ca�da de cabello').
elimina(ricino,caspa).
elimina(ricino,seborrea).

elimina(rosal,estre�imiento).
elimina(rosal,'c�licos infantles').
elimina(rosal,empachos).
elimina(rosal,conjuntivitis).

elimina(romero,'infecciones vaginales').
elimina(romero,'baja energ�a').
elimina(romero,'problemas mestruales').
elimina(romero,'mala digesti�n').
elimina(romero,estr�s).
elimina(romero,'ca�da de cabello').

elimina(ruda,'partos dif�ciles').
elimina(ruda,'hijos no deseados').
elimina(ruda,'mal humor en ciclo menstrual').
elimina(ruda,jaquecas).
elimina(ruda,'problemas mestruales').

elimina(simonillo,estre�imiento).
elimina(simonillo,ictericia).
elimina(simonillo,'catarro de v�a biliares').
elimina(simonillo,'c�lico hep�tico').
elimina(simonillo,'dispepsia cr�nica').

elimina(tamarindo,'problemas renales').
elimina(tamarindo,estre�imiento).
elimina(tamarindo,sed).
elimina(tamarindo,empachos).
elimina(tamarindo,'indigesti�n infantil').

elimina(tabach�n,'h�gado intoxicado').
elimina(tabach�n,'ri�on intoxicado').
elimina(tabach�n,'hijos no deseados').
elimina(tabach�n,'problemas mestruales').

elimina(taray,'problemas renales').
elimina(taray,'c�lculos renales').

elimina(tila,nerviosismo).
elimina(tila,'malestar general').
elimina(tila,'problemas de sue�o').
elimina(tila,'dolor hist�rico de cabeza').
elimina(tila,fiebre).
elimina(tila,'tos espasm�dica').
elimina(tila,estr�s).


%M�todo de preparaci�n de las plantas
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

%Lista de efectos de las plantas en el cuerpo
%*******************************************************************************************   
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


%Elementos de las plantas
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

%Origen de las plantas
%******************************************************************************************

origenPlanta(retema, 'noroeste de africa y la pen�nsula ib�rica').
origenPlanta(ricino, 'africa tropical').
origenPlanta(rosal, 'asia, persia y asia Menor.').
origenPlanta(romero, 'francia').
origenPlanta(ruda, 'europa').
origenPlanta(simonillo, 'Desconocido, pero se encuentra en mexico, colombia, peru, estados unidos, guatemala, ecuador.').
origenPlanta(tamarindo, 'india').
origenPlanta(tabach�n, 'zonas templadas o calientes del pais').
origenPlanta(taray, 'Desconocido, pero se encuentra en mexico').
origenPlanta(tila, 'europa, se da tambien en los bosques mexicanos').

%*******************************************************************************************



%Reglas y relaciones
%*******************************************************************************************
resumenDePlanta(Planta,Y,Z):-nombreCientifico(Planta,Y),elimina(Planta,Z).
buscarPlantaPorEnfermedad(Enfermedad,X):-elimina(X,Enfermedad).

%�Cuales son plantas o plantas medicinales?
listaDePlantas(X):-planta(X).

%�Qu� elementos se encuentran en las plantas?
elementosDeTodasLasPlanta(X,Y):-parteDePlanta(X,Y).

%�Qu� elementos tiene una planta en especifica? 
elementosDeCadaPlanta(Planta,X):-parteDePlanta(Planta,X).

%�Qu� plantas producen medicamentos?
buscarPlantaProdMedicamento(X,Y):-plantasProdMedic(X,Y).

%�Qu� medicamentos producen una planta en especifico?
buscarCadaPlantaProdMedicamento(Planta,Y):-plantasProdMedic(Planta,Y).
%cacao, toloache, peyote

%�Qu� medicamentos provienen de plantas?
buscarMedicamentosDePlantas(Medicamento,Y):-plantasProdMedic(Y,Medicamento).
%mescalina teobromina

%�Cuales son las acciones o efectos que tienen las plantas?
buscarEfectosDeLasPlantas(X,_):-efectoPlanta(X,_).

%Significado de palabras que son acciones o efectos de plantas sobre organismo
buscarSignificadoDeEfectosDePlantas(X,Y):-efectoPlanta(X,Y).

%Listado de plantas y sus acciones o efectos sobre el organismo
buscarEfectosPlanta(X,Y):-efecto(X,Y).

%�Acciones o efectos de una planta en especifico
buscarEfectosDeCadaPlanta(Planta,Y):-efecto(Planta,Y).

%�Qu� plantas son analg�sicas?
plantasAnalgesica(Planta):-efecto(Planta,analg�sica).

%Listar plantas medicinales y su nombre cient�fico
buscarNombreCient�fico(Planta,X):-nombreCientifico(Planta,X).

%�Cuales son las  enfermedades que curan las plantas?.
cura(X,Y):-planta(X), elimina(X,Y).

%�Cuales son  las enfermedades que que cura una planta en espec�fico? Zabila
curaDeCadaPlanta(Planta,Y):-planta(Planta), elimina(Planta,Y).

%�Cuales son las plantas que curan el herpes?
plantasCuranHerpes(Planta):-elimina(Planta,herpes).

%�Cuales son las formas de preparaci�n para tratamiento de enfermedades con uso de plantas?
modoDePreparacionPorEnfermedad(Planta,X,Y,Z):-elimina(Planta,X),preparacionDePlanta(Planta,Y),uso(Planta,Z).

%�Cuales son los modos de preparaci�n de una planta en especifico?
modoDePreparacion(Planta,Z,W,Y):-uso(Planta,Y),preparacionDeHiervas(Y,Z),preparacionDePlanta(Planta,W).

%�Cuales son los origenes de las plantas medicinales?
buscarOrigePlanta(X,Y):-origenPlanta(X,Y).

%�Cual es el origen de una planta?
buscarOrigenDeCadaPlanta(Planta,X):-origenPlanta(Planta,X).

%Botiqu�n de plantas
buscarEnBotiquin(Planta):-planta(Planta),botiquin(Planta).