@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cambiarasociado.p
    Purpose     : 

    Syntax      : /CambiarAsociado

    Description : Cambiar Asociado Modulo HU02 Depositos Pendientes por Aplicar

    Author(s)   : sis10
    Created     : Thu Dec 05 00:13:01 CST 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */
    DEFINE BUFFER bf-Cliente  FOR Cliente. 
    DEFINE BUFFER bf-DepBanco FOR DepBanco.
    DEFINE VARIABLE l-tpCte AS INTEGER NO-UNDO.
    DEFINE VARIABLE l-AsociadoNuevo LIKE Cliente.Id-Cliente NO-UNDO.
    DEFINE VARIABLE l-hora  AS INTEGER NO-UNDO.
    
    DEFINE TEMP-TABLE tt-DepBanco LIKE DepBanco
    FIELD RazonSocial LIKE Cliente.RazonSocial. 

/* ***************************  Main Block  *************************** */
/*_ -----------------------------------------
    Procedure Cambia Cliente
----------------------------------------- _*/ 
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE p-CambiaAsociado: 
    
   DEFINE INPUT  PARAMETER AsociadoNuevo LIKE Cliente.Id-Cliente NO-UNDO.
   DEFINE INPUT  PARAMETER Rec           AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER Respuesta     AS CHARACTER.  
 
    FIND FIRST DepBanco WHERE RECID(DepBanco) = Rec EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE DepBanco THEN DO:
        ASSIGN Respuesta = "El registro no está disponible para actualización.".
        RETURN.
    END.  
    
    /* Verificar si ya está actualizado */ 
    IF DepBanco.Id-Cliente = AsociadoNuevo THEN DO:
        ASSIGN Respuesta = "El Asociado ya está actualizado con el nuevo valor.".
        RETURN.
    END.
    
    FIND FIRST Depbanco WHERE RECID(DepBanco)= Rec NO-LOCK NO-ERROR.
    IF AVAILABLE DepBanco THEN DO:
        CREATE tt-DepBanco.
        ASSIGN tt-Depbanco.Id-Banco   = DepBanco.Id-Banco
               tt-DepBanco.Id-Cliente = DepBanco.Id-Cliente
               tt-DepBanco.FecDep     = DepBanco.FecDep
               tt-DepBanco.Importe    = DepBanco.Importe
               tt-DepBanco.Referencia = DepBanco.Referencia
               tt-DepBanco.HoraDep    = DepBanco.HoraDep
               tt-DepBanco.TipoCte    = DepBanco.TipoCte.
     END.
     FIND FIRST tt-DepBanco NO-LOCK NO-ERROR.
     
     
    FIND FIRST Asociado WHERE Asociado.Id-Cliente = tt-DepBanco.Id-Cliente
                          AND Asociado.Id-Asociado = AsociadoNuevo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE (Asociado) THEN DO:
       ASSIGN  Respuesta = "Asociado No Valido .".
        RETURN.
    END. 
        
    /********* Inicia la actualizacion *******/
    IF AVAILABLE (Asociado) THEN DO:       
            FIND FIRST bf-Cliente WHERE bf-Cliente.Id-Cliente = AsociadoNuevo NO-LOCK NO-ERROR.
            IF AVAILABLE bf-Cliente THEN DO:
                IF bf-Cliente.Id-Cliente = 3 THEN l-tpCte = 4.    // CONTADO
                ELSE DO:
                    FIND FIRST Zona OF Cliente NO-LOCK NO-ERROR.
                    IF AVAILABLE Zona THEN DO:
                        IF Zona.Ubic = 1 THEN ASSIGN l-tpCte = 1.         // LOCAL
                        ELSE IF Zona.Ubic <> 1 THEN ASSIGN l-tpCte = 2.   // FORANEO
                        IF bf-Cliente.Id-Resp = 30 THEN ASSIGN l-tpCte = 3.  // CORPORATIVO
                    END.
                END.
            END.
            // 2020-01-13 - Update de Reviso si el cliente es de cr�dito para reasignar su TipoCliente
            FIND FIRST MovCliente WHERE MovCliente.id-cliente = AsociadoNuevo NO-LOCK NO-ERROR.
            IF (NOT AVAILABLE MovCliente) THEN l-tpCte = 4.                
            ELSE DO:
                FIND FIRST MovCliente WHERE MovCliente.id-cliente = Cliente.Id-Cliente
                                        AND MovCliente.id-mc = 1
                                        AND MovCliente.saldo > 0
                                        NO-LOCK NO-ERROR.
                IF (NOT AVAILABLE MovCliente AND Cliente.limite = 0) THEN l-tpCte = 4.                
            END. 
            
            
            
            
            FIND FIRST tt-Depbanco NO-LOCK NO-ERROR.     
            
            /* Validar tipo de asociado antes de realizar la actualización */
            IF tt-DepBanco.TipoCte <> l-tpCte THEN DO:
                ASSIGN Respuesta = "El Asociado seleccionado tiene un tipo de cliente diferente.".
                DELETE tt-DepBanco. /* Elimina el registro temporal si el tipo de cliente no coincide */
                RETURN.  /* Termina el proceso sin realizar actualizaciones */
            END.
             //  l-hora = INTEGER(REPLACE(tt-DepBanco.HoraDep, ":", "")).
            // Busco el registro de la tabla temporal para actualizar el cliente
            FIND FIRST bf-DepBanco WHERE bf-DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
                                         bf-DepBanco.Id-Cliente = tt-DepBanco.Id-Cliente AND  
                                         bf-DepBanco.FecDep     = tt-DepBanco.FecDep AND 
                                         bf-DepBanco.Importe    = tt-DepBanco.Importe AND
                                         bf-DepBanco.Referencia = tt-DepBanco.Referencia AND
                                         NOT bf-DepBanco.Conciliado
                                         EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE bf-DepBanco AND bf-DepBanco.HoraDep = tt-DepBanco.HoraDep THEN DO:
                // Error-Excepcion - Busco antes si existe registro que quedar� al hacer Buffer-Copy
                FIND FIRST DepBanco WHERE DepBanco.Id-Banco   = tt-DepBanco.Id-Banco AND
                                          DepBanco.Id-Cliente = AsociadoNuevo AND  
                                          DepBanco.FecDep     = tt-DepBanco.FecDep AND 
                                          DepBanco.Importe    = tt-DepBanco.Importe AND
                                          DepBanco.Referencia = tt-DepBanco.Referencia AND
                                          NOT DepBanco.Conciliado
                                          NO-LOCK NO-ERROR.
                IF AVAILABLE DepBanco AND DepBanco.HoraDep = tt-DepBanco.HoraDep THEN DO:
                    ASSIGN  Respuesta ='Error, NO se puede continuar. \n Ya existe un registro previo con este Asociado'.
                    UNDO, NEXT.
                END. 
                ELSE DO:
                    ASSIGN 
                        bf-DepBanco.Activo      = FALSE
/*                        bf-DepBanco.Id-User     = CAPS(IdUsuario)*/
                        bf-DepBanco.FecAplica   = DATETIME(TODAY, MTIME).
                        
                    BUFFER-COPY bf-DepBanco EXCEPT HoraDep TO DepBanco NO-ERROR.
                        
                    ASSIGN DepBanco.Id-Cliente  = AsociadoNuevo
                           DepBanco.HoraDep     = tt-DepBanco.HoraDep
                           DepBanco.Activo      = TRUE
                           DepBanco.Id-User     =  ''
                           DepBanco.FecAplica   = ?
                           DepBanco.TipoCte     = l-tpCte
                           tt-DepBanco.Id-Cliente = AsociadoNuevo
                           tt-DepBanco.RazonSocial = bf-Cliente.RazonSocial. 
                END.
            END.
            
            RELEASE bf-DepBanco.
            RELEASE DepBanco.
            
            IF tt-DepBanco.TipoCte <> l-tpCte THEN DO:
                ASSIGN  Respuesta = 'El Asociado seleccionado tiene un tipo de cliente diferente'.
                DELETE tt-DepBanco.
            END.
            ELSE  
                ASSIGN  Respuesta = 'Asociado modificado. \n Se actualizara la informacion.'.
        END.
        ELSE LEAVE.
    END.