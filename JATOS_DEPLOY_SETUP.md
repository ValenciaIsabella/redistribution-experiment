# Guía: Linkear un nuevo repositorio GitHub con un nuevo estudio en JATOS mindprobe

Este documento es una guía operacional para configurar el deploy automático de archivos HTML desde un repositorio GitHub hacia un estudio en [JATOS mindprobe](https://mindprobe.eu). Está escrita para ser usada por humanos o por una IA como input de contexto.

---

## Qué hace este setup

Cada vez que haces `git push` a `main` y hay cambios en archivos `.html`, GitHub Actions sube automáticamente esos archivos al directorio de assets del estudio correspondiente en JATOS usando la API REST.

---

## Información que necesitas recopilar antes de empezar

### 1. `STUDY_ID` — ID numérico del estudio en JATOS

**Qué es:** Un número entero que identifica tu estudio dentro de JATOS.

**Dónde encontrarlo:**
1. Entra a [https://mindprobe.eu](https://mindprobe.eu) con tu cuenta.
2. En el panel principal, busca tu estudio en la lista.
3. Haz clic en el estudio para abrirlo.
4. Mira la URL del navegador — tendrá la forma:
   ```
   https://mindprobe.eu/jatos/gui/study/{STUDY_ID}/...
   ```
5. El número en esa URL es tu `STUDY_ID`.

**Alternativa vía API:**
```bash
curl -s \
  -H "Authorization: Bearer TU_TOKEN" \
  -H "accept: application/json" \
  "https://mindprobe.eu/jatos/api/v1/studies/properties" \
  | python3 -m json.tool
```
Busca el campo "id" del estudio que te interesa.

**Ejemplo:** `23660`

---

### 2. `JATOS_REMOTE_URL` — URL base de JATOS mindprobe

**Qué es:** La URL raíz del servidor JATOS, sin slash al final.

**Valor fijo para mindprobe:**
```
https://mindprobe.eu
```

Este valor va como GitHub Secret en el repositorio.

---

### 3. `JATOS_REMOTE_TOKEN` — Token de API personal

**Qué es:** Un token Bearer que autentica tus llamadas a la API de JATOS. Es equivalente a una contraseña — trátalo como secreto.

**Dónde generarlo:**
1. Entra a [https://mindprobe.eu](https://mindprobe.eu).
2. Haz clic en tu nombre de usuario (esquina superior derecha) → **My profile** (o **API Tokens**).
3. En la sección **API tokens**, haz clic en **Create token** (o **New token**).
4. Dale un nombre descriptivo (ej. `github-deploy-redistribution`).
5. Copia el token inmediatamente — **solo se muestra una vez**.

> ⚠️ Los tokens creados via interfaz web no tienen expiración por defecto. Los creados via API expiran en 1 hora. Usa siempre la interfaz web para tokens de deploy.

---

## Pasos para configurar un nuevo repositorio

### PASO 1 — Crear el repositorio en GitHub

1. Crea un repositorio nuevo en GitHub (puede ser público o privado).
2. Asegúrate de que los archivos HTML del experimento estén en la **raíz del repositorio** (no en subdirectorios), ya que el workflow busca `*.html` en la raíz.

---

### PASO 2 — Agregar los GitHub Secrets

Los secrets son variables de entorno cifradas que usa el workflow.

1. En GitHub, ve a tu repositorio → **Settings** → **Secrets and variables** → **Actions**.
2. Haz clic en **New repository secret** y agrega los siguientes dos secrets:

| Nombre del secret      | Valor                          |
|------------------------|-------------------------------|
| `JATOS_REMOTE_URL`     | `https://mindprobe.eu`        |
| `JATOS_REMOTE_TOKEN`   | el token que generaste en JATOS |

> ⚠️ Los nombres deben ser exactamente esos (mayúsculas y guiones bajos), ya que el workflow los referencia con esos nombres.

---

### PASO 3 — Crear el workflow de GitHub Actions

Crea el archivo `.github/workflows/deploy.yml` en tu repositorio con el siguiente contenido exacto. Solo necesitas cambiar el valor de `STUDY_ID`:

```yaml
name: Deploy to JATOS

on:
  push:
    branches:
      - main
    paths:
      - '**.html'

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repo
        uses: actions/checkout@v4

      - name: Upload HTML files to JATOS mindprobe
        env:
          JATOS_REMOTE_URL: ${{ secrets.JATOS_REMOTE_URL }}
          JATOS_REMOTE_TOKEN: ${{ secrets.JATOS_REMOTE_TOKEN }}
        run: |
          STUDY_ID="REEMPLAZA_CON_TU_STUDY_ID"

          echo "Base URL: $JATOS_REMOTE_URL"
          echo "Study ID: $STUDY_ID"

          shopt -s nullglob
          html_files=(*.html)

          if [ ${#html_files[@]} -eq 0 ]; then
            echo "No .html files found in the repository root — nothing to upload."
            exit 1
          fi

          for file in "${html_files[@]}"; do
            FILENAME=$(basename "$file")
            echo ""
            echo "Uploading $FILENAME..."

            HTTP_CODE=$(curl -s -o /tmp/response.txt -w "%{http_code}" \
              -X POST \
              -H "Authorization: Bearer $JATOS_REMOTE_TOKEN" \
              -H "accept: application/json" \
              -F "studyAssetsFile=@$file" \
              "$JATOS_REMOTE_URL/jatos/api/v1/studies/$STUDY_ID/assets/$FILENAME")

            echo "HTTP Status: $HTTP_CODE"
            cat /tmp/response.txt
            echo ""

            if [ "$HTTP_CODE" -lt 200 ] || [ "$HTTP_CODE" -ge 300 ]; then
              echo "FAILED: $FILENAME (HTTP $HTTP_CODE)"
              exit 1
            else
              echo "OK: $FILENAME uploaded successfully"
            fi
          done
```

> 🔑 El único cambio obligatorio es reemplazar `REEMPLAZA_CON_TU_STUDY_ID` con el ID numérico de tu estudio (ej. `23660`).

---

### PASO 4 — Verificar que el estudio existe en JATOS

Antes del primer deploy, confirma manualmente que el estudio con ese `STUDY_ID` existe y que tu token tiene acceso:

```bash
curl -s \
  -H "Authorization: Bearer TU_TOKEN" \
  -H "accept: application/json" \
  "https://mindprobe.eu/jatos/api/v1/studies/TU_STUDY_ID/properties" \
  | python3 -m json.tool
```

Deberías recibir un JSON con las propiedades del estudio (título, UUID, componentes, etc.). Si recibes `401` o `404`, revisa el token o el ID.

---

### PASO 5 — Hacer el primer push

1. Haz cualquier cambio en un archivo `.html` en la raíz del repositorio.
2. Haz commit y push a `main`.
3. Ve a la pestaña **Actions** en GitHub para ver el workflow ejecutándose.
4. Si todo está bien, verás algo así en los logs:
   ```
   Uploading experiment.html...
   HTTP Status: 200
   {"apiVersion":"...","message":"File uploaded successfully"}
   OK: experiment.html uploaded successfully
   ```

---

## Checklist de verificación rápida

Antes de hacer el primer deploy a un nuevo repo/estudio, confirma:

- [ ] El estudio existe en JATOS mindprobe y conoces su `STUDY_ID`
- [ ] Tienes un token de API válido generado desde la interfaz web de mindprobe
- [ ] El secret `JATOS_REMOTE_URL` está configurado en el repositorio GitHub
- [ ] El secret `JATOS_REMOTE_TOKEN` está configurado en el repositorio GitHub
- [ ] El archivo `.github/workflows/deploy.yml` existe con el `STUDY_ID` correcto
- [ ] Los archivos HTML están en la raíz del repositorio (no en subdirectorios)
- [ ] El estudio en JATOS tiene al menos un componente creado (para que los assets tengan destino)

---

## Errores comunes y soluciones

| Síntoma | Causa probable | Solución |
|--------|---------------|---------|
| HTTP `401` | Token inválido o expirado | Regenerar token en mindprobe y actualizar el secret |
| HTTP `404` | `STUDY_ID` incorrecto | Verificar el ID en la URL de JATOS o via API |
| HTTP `200` pero no se actualiza en JATOS | Antes era por usar `file=` en vez de `studyAssetsFile=` | Confirmar que el campo del formulario es `studyAssetsFile` |
| `No .html files found` | Los HTML no están en la raíz del repo | Mover los archivos `.html` a la raíz |
| El workflow no se dispara | No hay cambios en archivos `.html` | El trigger solo activa con cambios en `**.html`; editar un HTML o cambiar el filtro del trigger |

---

## Referencia técnica

- **Endpoint usado:** `POST /jatos/api/v1/studies/{id}/assets/{filepath}`
- **Campo multipart requerido:** `studyAssetsFile` (nombre exacto definido en la API de JATOS)
- **Comportamiento:** Si el archivo ya existe, lo **sobreescribe**. Si no existe, lo **crea**.
- **Autenticación:** Bearer token en header `Authorization`
- **Spec oficial de la API:** [`jatos-api.yaml` en JATOS/JATOS](https://github.com/JATOS/JATOS/blob/main/jatos-api.yaml)

---

## Repositorio de referencia

Este setup está implementado y funcionando en:
- **Repositorio:** [ValenciaIsabella/redistribution-experiment](https://github.com/ValenciaIsabella/redistribution-experiment)
- **Workflow:** [.github/workflows/deploy.yml](https://github.com/ValenciaIsabella/redistribution-experiment/blob/main/.github/workflows/deploy.yml)

Puedes usar ese repositorio como plantilla.