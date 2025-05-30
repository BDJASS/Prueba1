@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*
    Empresa : ADOSA
    Programa: RespuestaClientes.p
              /RespuestaClientes
    Fucnion : Consulta Historial Respuesta / Guarda Respuestas
    Autor   : ALEX
    Fecha   : 3 de Diciembre DEL 2024
*/


DEFINE TEMP-TABLE ttDatos
    FIELD IdCliente LIKE HistRespCte.Id-Cliente
    FIELD IdUser    LIKE HistRespCte.Id-User
    FIELD NomUser   LIKE Usuario.Nom-Usuario
    FIELD Coment    LIKE HistRespCte.Coment
    FIELD FecReg    LIKE HistRespCte.FecReg 
    FIELD Hora      AS CHARACTER.

/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaRespuesta:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
        Empresa : ADOSA
        Programa: leehistrespcte.p
        Fucnion : Lee tabla de historial de respuestas de clientes
        Autor   : ALEX
        Fecha   : 3 de Diciembre DEL 2024
    */


    DEFINE INPUT PARAMETER ipCliente AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttDatos.
    DEFINE OUTPUT PARAMETER opRazonSocial LIKE Cliente.RazonSocial NO-UNDO.
    DEFINE OUTPUT PARAMETER opError AS CHARACTER NO-UNDO.

    FIND Cliente WHERE Cliente.Id-Cliente = ipCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente THEN 
    DO:
        opError = "Cliente inexistente.".
        RETURN.    
    END.
    
    ASSIGN 
        opRazonSocial = Cliente.RazonSocial.

    FOR EACH HistRespCte WHERE HistRespCte.Id-Cliente = ipCliente NO-LOCK:
        
        FIND Usuario WHERE Usuario.Id-User = HistRespCte.Id-User NO-LOCK NO-ERROR.
        CREATE ttDatos.
        ASSIGN 
            ttDatos.IdCliente = HistRespCte.Id-Cliente
            ttDatos.IdUser    = HistRespCte.Id-User
            ttDatos.NomUser   = Usuario.Nom-Usuario WHEN AVAILABLE Usuario
            ttDatos.Coment    = HistRespCte.Coment
            ttDatos.FecReg    = HistRespCte.FecReg
            ttDatos.Hora      = STRING(HistRespCte.HorReg,"HH:MM:SS").
    END.

    RETURN.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostGuardaRespuesta:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
        Empresa : ADOSA
        Programa: grabahistrespcte.p
        Fucnion : Graba tabla HistRespCte
        Autor   : ALEX
        Fecha   : 3 de Diciembre DEL 2024
    */

    DEFINE INPUT PARAMETER TABLE FOR ttDatos.
    DEFINE OUTPUT PARAMETER opMensaje AS CHARACTER NO-UNDO.

    FOR EACH ttDatos NO-LOCK:
        CREATE HistRespCte.
        ASSIGN 
            HistRespCte.Id-Cliente = ttDatos.IdCliente
            HistRespCte.Id-User    = ttDatos.IdUser
            HistRespCte.Coment     = ttDatos.Coment
            HistRespCte.FecReg     = TODAY 
            HistRespCte.HorReg     = TIME.
    END.
 
    ASSIGN 
        opMensaje = "Terminado".

    RETURN.
END PROCEDURE.
