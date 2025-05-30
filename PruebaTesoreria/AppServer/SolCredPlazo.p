@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : SolCredPlazo.p
    Purpose     : Servicios para actualizar y consultar datos relacionados con crédito en la tabla Cliente.
    URL         : /ClienteCreditoPlazo
    Module      : Gestión de Créditos
------------------------------------------------------------------------*/

/* ***************************  Definitions **************************** */

/* Tabla temporal para entrada (POST) */
DEFINE TEMP-TABLE ttClienteInput NO-UNDO
    FIELD IdCliente       AS INTEGER    /* ID del cliente */ 
    FIELD IdUser          AS CHAR  
    FIELD LineaCredito    AS DECIMAL    /* Límite de crédito */
    FIELD PlazoAutorizado AS INTEGER    /* Plazo autorizado */
    FIELD IdClase         AS INTEGER    /* Clase del cliente */
    FIELD IdCalidad       AS INTEGER   /* Calidad del cliente */
    FIELD IdCobrador      LIKE Cliente.Id-Cobrador.

/* Tabla temporal para salida (POST y GET) */
DEFINE TEMP-TABLE ttClienteOutput NO-UNDO
    FIELD IdCliente       AS INTEGER    /* ID del cliente */
    FIELD LineaCredito    AS DECIMAL    /* Límite de crédito */
    FIELD PlazoAutorizado AS INTEGER    /* Plazo autorizado */
    FIELD IdClase         AS INTEGER    /* Clase del cliente */
    FIELD IdCalidad       AS INTEGER   /* Calidad del cliente */
    FIELD IdCobrador      LIKE Cliente.Id-Cobrador.
/* Buffer para la tabla persistente */
DEFINE BUFFER bfCliente FOR Cliente.

/* ***************************  Main Procedures **************************** */

/* -------------------------------------------------------------------------- */
/* POST: Servicio para actualizar los datos de crédito en la tabla Cliente */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ActualizarDatosCredito:
    DEFINE INPUT  PARAMETER TABLE FOR ttClienteInput.  /* Tabla temporal de entrada */
    DEFINE OUTPUT PARAMETER l-mensaje AS CHARACTER.    /* Paramentro de salida para mensaje */
    
    /* Inicializar el mensaje como vacío al inicio */
    ASSIGN 
        l-mensaje = "".
    
    EMPTY TEMP-TABLE ttClienteOutput.

    /* Procesar cada registro en la tabla temporal */
    FOR EACH ttClienteInput:
        
        /* Validar que el ID del cliente sea obligatorio */
        IF ttClienteInput.IdCliente = ? THEN 
        DO:
            ASSIGN 
                l-mensaje = "El campo IdCliente es obligatorio en la tabla de entrada.".
            RETURN.
        END.
        
        /* Validar que el cliente exista en la tabla Cliente */
        FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = ttClienteInput.IdCliente EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bfCliente THEN 
        DO:
            ASSIGN 
                l-mensaje = "El cliente con ID " + STRING(ttClienteInput.IdCliente) + " no existe en la base de datos.".
            RETURN.
        END.
        
        IF ttClienteInput.IdUser = "" THEN
        DO:
            ASSIGN 
                l-Mensaje = "Ingresar User que realiza la modificacion".
            RETURN.
        END.

        /* Actualizar solo los campos que tienen información en ttClienteInput */
        IF ttClienteInput.LineaCredito <> ? THEN 
        DO:
     
            CREATE CambioCte.
            ASSIGN
                CambioCte.Id-Cliente = bfCliente.Id-Cliente
                CambioCte.Id-User    = ttClienteInput.IdUser    
                CambioCte.Descr      = "limite"
                CambioCte.ValorNuevo = STRING(ttClienteInput.LineaCredito)
                CambioCte.ValorOld   = STRING(bfCliente.Limite)
                CambioCte.FecReg     = TODAY
                CambioCte.Hora       = TIME    
                CambioCte.Campo      = 500.  
            ASSIGN 
                bfCliente.Limite = ttClienteInput.LineaCredito.              
        END. 
            

        IF ttClienteInput.PlazoAutorizado <> ? THEN 
        DO:
            CREATE CambioCte.
            ASSIGN
                CambioCte.Id-Cliente = bfCliente.Id-Cliente
                CambioCte.Id-User    = ttClienteInput.IdUser    
                CambioCte.Descr      = "Plazo"
                CambioCte.ValorNuevo = STRING(ttClienteInput.PlazoAutorizado)
                CambioCte.ValorOld   = STRING(bfCliente.Plazo)
                CambioCte.FecReg     = TODAY
                CambioCte.Hora       = TIME    
                CambioCte.Campo      = 501.
            ASSIGN 
                bfCliente.Plazo = ttClienteInput.PlazoAutorizado.
        END.
        
        IF ttClienteInput.IdClase <> ? THEN 
        DO:
            /* Crear registro de CambioCte */
            CREATE CambioCte.
            ASSIGN
                CambioCte.Id-Cliente = bfCliente.Id-Cliente
                CambioCte.Id-User    = ttClienteInput.IdUser    
                CambioCte.Descr      = "Id-ClaseCte"
                CambioCte.ValorNuevo = STRING(ttClienteInput.IdClase)
                CambioCte.ValorOld   = STRING(bfCliente.Id-ClaseCte)
                CambioCte.FecReg     = TODAY
                CambioCte.Hora       = TIME    
                CambioCte.Campo      = 250.
    
            /* Asignar Id-ClaseCte */
            bfCliente.Id-ClaseCte = ttClienteInput.IdClase.
    
            /* Asignar Id-Resp según la clase */
            CASE ttClienteInput.IdClase:
                WHEN 1 THEN 
                    bfCliente.Id-Resp = 33.  /* Clase 1 -> Responsable 33 */
                WHEN 2 THEN 
                    bfCliente.Id-Resp = 9.   /* Clase 2 -> Responsable 9 */
                WHEN 3 THEN 
                    bfCliente.Id-Resp = 30.  /* Clase 3 -> Responsable 30 */
            END CASE.
        END.   
        
        IF ttClienteInput.IdCalidad <> ? THEN DO:
            
            CREATE CambioCte.
            ASSIGN
                CambioCte.Id-Cliente = bfCliente.Id-Cliente
                CambioCte.Id-User    = ttClienteInput.IdUser    
                CambioCte.Descr      = "Id-Calidad"
                CambioCte.ValorNuevo = STRING(ttClienteInput.IdCalidad)
                CambioCte.ValorOld   = STRING(bfCliente.Id-Calidad)
                CambioCte.FecReg     = TODAY
                CambioCte.Hora       = TIME    
                CambioCte.Campo      = 505.
            ASSIGN 
            
            bfCliente.Id-Calidad = ttClienteInput.IdCalidad.
        END.    
        
        IF ttClienteInput.IdCobrador <> ? THEN DO:
            CREATE CambioCte.
            ASSIGN
                CambioCte.Id-Cliente = bfCliente.Id-Cliente
                CambioCte.Id-User    = ttClienteInput.IdUser    
                CambioCte.Descr      = "Id-Cobrador"
                CambioCte.ValorNuevo = STRING(ttClienteInput.IdCobrador)
                CambioCte.ValorOld   = STRING(bfCliente.Id-Cobrador)
                CambioCte.FecReg     = TODAY
                CambioCte.Hora       = TIME    
                CambioCte.Campo      = 15.
            ASSIGN 
            bfCliente.Id-Cobrador = ttClienteInput.IdCobrador.
        END.
        /* Asignar mensaje de éxito al actualizar los datos */
        ASSIGN 
            l-mensaje = "Datos actualizados correctamente para el cliente con ID: " + STRING(ttClienteInput.IdCliente) + ".".
    END.

    /* Liberar buffer */
    RELEASE bfCliente.  
END PROCEDURE.   

/* -------------------------------------------------------------------------- */
/* GET: Servicio para consultar los datos de crédito de un cliente */
/* -------------------------------------------------------------------------- */
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE ConsultarDatosCredito:
    DEFINE INPUT  PARAMETER IdCliente AS INTEGER NO-UNDO. /* ID del cliente */
    DEFINE OUTPUT PARAMETER TABLE FOR ttClienteOutput.    /* Tabla temporal de salida */

    EMPTY TEMP-TABLE ttClienteOutput.

    /* Buscar el registro en la tabla Cliente */
    FIND FIRST bfCliente WHERE bfCliente.Id-Cliente = IdCliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfCliente THEN 
    DO:
        RETURN ERROR "El cliente con ID " + STRING(IdCliente) + " no existe en la base de datos.".
    END.

    /* Copiar los datos encontrados a la tabla temporal de salida */
    CREATE ttClienteOutput.
    ASSIGN
        ttClienteOutput.IdCliente       = bfCliente.Id-Cliente
        ttClienteOutput.LineaCredito    = bfCliente.Limite
        ttClienteOutput.PlazoAutorizado = bfCliente.Plazo
        ttClienteOutput.IdClase         = bfCliente.Id-ClaseCte
        ttClienteOutput.IdCalidad       = bfCliente.Id-Calidad
        ttClienteOutPut.IdCobrador      = bfCliente.Id-Cobrador.

    /* Liberar buffer */
    RELEASE bfCliente.
END PROCEDURE.
