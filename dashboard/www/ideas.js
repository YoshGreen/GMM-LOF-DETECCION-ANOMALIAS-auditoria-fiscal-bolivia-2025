// =============================================================================
// IDEAS.JS — SISTEMA INTERACTIVO DE EXPEDIENTES FORENSES (Ideas/)
// - Menú Lateral Desplegable (Off-Canvas Forensic Drawer) con Pinning
// - Alternador de Tema: Oscuro (Dark Telemetry) / Claro (Investigador Cork)
// - Tablero de Corcho con Hilos Rojos Dinámicos
// - Tablero Split-Flap con Giro Mecánico de Caracteres
// - Cinta de Sumadora Contable Secuencial
// =============================================================================

// 1. MENÚ LATERAL DESPLEGABLE (OFF-CANVAS FORENSIC DRAWER)
function toggleForensicDrawer() {
  const body = document.body;
  const isOpen = body.classList.toggle("drawer-open");
  const btn = document.getElementById("btnDrawerOpen");
  
  if (btn) {
    btn.setAttribute("aria-expanded", isOpen ? "true" : "false");
    btn.innerHTML = isOpen ? "✕ Cerrar Menú" : "☰ Menú de Expedientes";
  }

  // Notificar a Plotly para recalcular dimensiones responsivas
  setTimeout(() => {
    window.dispatchEvent(new Event("resize"));
    if (typeof renderCorkStrings === "function") renderCorkStrings();
  }, 320);
}

function closeForensicDrawer() {
  const body = document.body;
  if (body.classList.contains("drawer-pinned")) return; // Si está fijado en desktop, no cerrar

  body.classList.remove("drawer-open");
  const btn = document.getElementById("btnDrawerOpen");
  if (btn) {
    btn.setAttribute("aria-expanded", "false");
    btn.innerHTML = "☰ Menú de Expedientes";
  }

  setTimeout(() => {
    window.dispatchEvent(new Event("resize"));
    if (typeof renderCorkStrings === "function") renderCorkStrings();
  }, 320);
}

// Compatibilidad por si se llama con el nombre antiguo
function toggleSidebarMenu() {
  toggleForensicDrawer();
}

function togglePinDrawer() {
  const body = document.body;
  const isPinned = body.classList.toggle("drawer-pinned");
  const btn = document.getElementById("btnPinDrawer");
  
  if (btn) {
    btn.innerHTML = isPinned ? "📌 Menú Fijado" : "📍 Fijar en Pantalla";
    btn.classList.toggle("active", isPinned);
  }

  try {
    localStorage.setItem("gam_drawer_pinned", isPinned ? "true" : "false");
  } catch(e) {}

  setTimeout(() => {
    window.dispatchEvent(new Event("resize"));
    if (typeof renderCorkStrings === "function") renderCorkStrings();
  }, 320);
}

// 2. ALTERNADOR DE TEMA (OSCURO / CLARO)
function toggleDashboardTheme() {
  const body = document.body;
  const btn = document.getElementById("btnThemeToggle");
  const isLight = body.classList.toggle("theme-light");
  
  try {
    localStorage.setItem("gam_audit_theme", isLight ? "light" : "dark");
  } catch(e) {}

  if (btn) {
    btn.innerHTML = isLight ? "🌙 Modo Oscuro" : "☀️ Modo Investigador";
  }

  setTimeout(() => {
    window.dispatchEvent(new Event("resize"));
    if (typeof renderCorkStrings === "function") renderCorkStrings();
  }, 220);
}

// 3. TABLERO DE CORCHO CRIMINALÍSTICO (Hilos Rojos entre sospechosos)
function renderCorkStrings() {
  const cork = document.getElementById("corkBoard");
  const svg = document.getElementById("corkSvg");
  if (!cork || !svg) return;

  const rect = cork.getBoundingClientRect();
  if (rect.width === 0 || rect.height === 0) return;

  svg.setAttribute("viewBox", `0 0 ${rect.width} ${rect.height}`);
  
  const groups = {};
  const cards = cork.querySelectorAll(".cork-card");
  cards.forEach(card => {
    const grp = card.getAttribute("data-group");
    if (!grp) return;
    if (!groups[grp]) groups[grp] = [];
    groups[grp].push(card);
  });

  let pathsHtml = "";
  for (const grp in groups) {
    const arr = groups[grp];
    if (arr.length < 2) continue;
    for (let i = 0; i < arr.length - 1; i++) {
      const p1 = arr[i].getBoundingClientRect();
      const p2 = arr[i + 1].getBoundingClientRect();
      
      const x1 = p1.left + p1.width / 2 - rect.left;
      const y1 = p1.top - rect.top; // Chincheta superior
      const x2 = p2.left + p2.width / 2 - rect.left;
      const y2 = p2.top - rect.top;
      
      const sag = Math.max(y1, y2) + 38; // Curva de hilo colgante
      pathsHtml += `<path data-group="${grp}" d="M${x1},${y1} Q${(x1 + x2) / 2},${sag} ${x2},${y2}"/>`;
    }
  }
  svg.innerHTML = pathsHtml;
}

function initCorkEvents() {
  const cork = document.getElementById("corkBoard");
  if (!cork) return;

  const cards = cork.querySelectorAll(".cork-card");
  cards.forEach(card => {
    card.onmouseenter = () => {
      cork.classList.add("dim");
      const grp = card.getAttribute("data-group");
      cork.querySelectorAll(`[data-group="${grp}"]`).forEach(el => el.classList.add("hl"));
    };
    card.onmouseleave = () => {
      cork.classList.remove("dim");
      cork.querySelectorAll(".hl").forEach(el => el.classList.remove("hl"));
    };
  });
}

// 4. TABLERO SPLIT-FLAP (Telemetría Mecánica de Caracteres)
const FLAP_CHARS = "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ0123456789.%-";

function setFlapChar(el, targetChar, steps) {
  // Limpiar timer previo para evitar que caracteres sigan bailando o acumulándose
  if (el._flapTimer) {
    clearInterval(el._flapTimer);
    el._flapTimer = null;
  }

  // Si el objetivo es espacio en blanco, NO rotar ruido aleatorio para evitar vibración en huecos vacíos
  if (!targetChar || targetChar === " ") {
    el.textContent = " ";
    return;
  }

  let count = 0;
  el._flapTimer = setInterval(() => {
    count++;
    if (count >= steps) {
      clearInterval(el._flapTimer);
      el._flapTimer = null;
      el.textContent = targetChar;
    } else {
      el.textContent = FLAP_CHARS[Math.floor(Math.random() * FLAP_CHARS.length)];
    }
  }, 34);
}

function runFlapAnimation(onlyRowIndex) {
  const rows = document.querySelectorAll(".flap-row");
  if (!rows || rows.length === 0) return;

  rows.forEach((row, rowIndex) => {
    // Si se especificó una fila particular y no coincide, no reanimar esa fila
    if (onlyRowIndex !== undefined && onlyRowIndex !== null && rowIndex !== onlyRowIndex) {
      return;
    }

    const cells = row.querySelectorAll(".flap-cell");
    cells.forEach((cell, cellIndex) => {
      const targetChar = cell.getAttribute("data-char") || cell.textContent || " ";
      // Casillas con letras giran de 4 a 12 ciclos escalonados
      const steps = 4 + (cellIndex % 6) * 2 + (onlyRowIndex !== undefined ? 0 : Math.min(rowIndex, 6) * 2);
      setFlapChar(cell, targetChar, steps);
    });
  });
}

// 5. CINTA DE SUMADORA CONTABLE
let printSessionId = 0;
async function printReceiptSequence() {
  const paper = document.getElementById("receiptContent");
  if (!paper) return;

  const currentId = ++printSessionId;
  paper.innerHTML = "";

  const lines = [
    { type: "bold center", text: "AUDITORÍA FORENSE PRESUPUESTARIA" },
    { type: "center", text: "GAM DE BOLIVIA · GESTIÓN 2025" },
    { type: "small center", text: "MEFP / DUCKDB OLAP COLUMNAR" },
    { type: "dashed" },
    { type: "row", l: "GAM AUDITADOS", r: "335" },
    { type: "row", l: "PARTIDAS GASTO", r: "429.753" },
    { type: "row", l: "DEUDA FLOTANTE", r: "Bs 1.325 M" },
    { type: "dashed" },
    { type: "bold center", text: "— CONSENSO GMM + LOF —" },
    { type: "row bold", l: "01 CHULUMANI", r: "LOF: 2,64" },
    { type: "small", text: "EJEC 31,9% · DEUDA 16,2% · PARÁLISIS FISCAL" },
    { type: "row bold", l: "02 MOJINETE", r: "LOF: 1,96" },
    { type: "small", text: "EJEC 78,7% · SUPERÁVIT ATÍPICO 46,7%" },
    { type: "row bold", l: "03 TAHUA", r: "LOF: 1,91" },
    { type: "small", text: "EJEC 46,3% · INVERSIÓN PÚBLICA 0,0%" },
    { type: "row bold", l: "04 SANTA CRUZ", r: "LOF: 1,76" },
    { type: "small", text: "EJEC 80,0% · DEUDA 17,8% · ILIQUIDEZ" },
    { type: "row bold", l: "05 CAQUIAVIRI", r: "LOF: 1,70" },
    { type: "small", text: "VIOLACIÓN BENFORD (p=0,031) · DEUDA 16,0%" },
    { type: "row bold", l: "06 HUMANATA", r: "LOF: 1,70" },
    { type: "small", text: "EJEC 55,0% · AISLAMIENTO VECINDAD" },
    { type: "row bold", l: "07 SAN JUAN", r: "LOF: 1,69" },
    { type: "small", text: "EJEC 49,3% · 14,2% DUPLICADOS" },
    { type: "row bold", l: "08 ANTEQUERA", r: "LOF: 1,65" },
    { type: "small", text: "EJEC 67,0% · RIGIDEZ EN GRUPO 20000" },
    { type: "row bold", l: "09 MALLA", r: "LOF: 1,61" },
    { type: "small", text: "EJEC 65,8% · ALTA DISPERSIÓN" },
    { type: "row bold", l: "10 COLPA BÉLGICA", r: "LOF: 1,60" },
    { type: "small", text: "EJEC 89,1% · DESFASE DE FLUJO" },
    { type: "row bold", l: "11 BOYUIBE", r: "LOF: 1,54" },
    { type: "small", text: "19,86% PARTIDAS DUPLICADAS (FRACCIONAMIENTO)" },
    { type: "row bold", l: "12 COLCHA K", r: "LOF: 1,52" },
    { type: "small", text: "15,32% PARTIDAS DUPLICADAS" },
    { type: "dashed" },
    { type: "row bold", l: "TOTAL OBSERVADOS", r: "12 GAMs" },
    { type: "row", l: "% DEL UNIVERSO", r: "3,58 %" },
    { type: "stamp" },
    { type: "dashed" },
    { type: "barcode" },
    { type: "small center", text: "EXPEDIENTE OFICIAL DE AUDITORÍA" }
  ];

  for (const item of lines) {
    if (currentId !== printSessionId) return;
    const div = document.createElement("div");

    if (item.type === "dashed") {
      div.className = "receipt-line dashed";
    } else if (item.type === "barcode") {
      div.className = "receipt-barcode";
    } else if (item.type === "stamp") {
      div.className = "receipt-line";
      div.innerHTML = `<span>ESTADO FINAL</span><span class="receipt-stamp">OBSERVADO</span>`;
    } else if (item.type.includes("row")) {
      div.className = "receipt-line " + item.type;
      div.innerHTML = `<span>${item.l}</span><i></i><span>${item.r}</span>`;
    } else {
      div.className = "receipt-line " + item.type;
      div.textContent = item.text;
    }

    paper.appendChild(div);
    await new Promise(r => setTimeout(r, item.type === "dashed" ? 25 : 60));
  }
}

// 6. INICIALIZACIÓN Y EVENT LISTENERS RESPONSIVOS
window.addEventListener("resize", () => {
  renderCorkStrings();
});

// Cerrar drawer al presionar Escape
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape") {
    closeForensicDrawer();
  }
});

document.addEventListener("DOMContentLoaded", () => {
  // Cargar tema guardado
  try {
    const savedTheme = localStorage.getItem("gam_audit_theme");
    if (savedTheme === "light") {
      document.body.classList.add("theme-light");
      const btn = document.getElementById("btnThemeToggle");
      if (btn) btn.innerHTML = "🌙 Modo Oscuro";
    }
    
    // Cargar preferencia de menú fijado
    const savedPin = localStorage.getItem("gam_drawer_pinned");
    if (savedPin === "true" && window.innerWidth >= 1200) {
      document.body.classList.add("drawer-pinned");
      const pinBtn = document.getElementById("btnPinDrawer");
      if (pinBtn) {
        pinBtn.innerHTML = "📌 Menú Fijado";
        pinBtn.classList.add("active");
      }
    }
  } catch(e) {}

  setTimeout(() => {
    renderCorkStrings();
    initCorkEvents();
    runFlapAnimation();
    printReceiptSequence();
  }, 450);
});

// Al seleccionar cualquier pestaña del menú (evento click o tab de Bootstrap):
$(document).on("click", ".nav-pills a, .nav-pills .nav-link", function (e) {
  const tabEl = $(this);
  const activeTabTitle = tabEl.find("span:first-child").text() || tabEl.text();
  const indicator = document.getElementById("currentSectionBadge");
  if (indicator && activeTabTitle) {
    indicator.textContent = activeTabTitle.trim();
  }

  // Si no está fijado en desktop, cerrar el menú desplegable automáticamente
  if (!document.body.classList.contains("drawer-pinned")) {
    closeForensicDrawer();
  }

  setTimeout(() => {
    window.dispatchEvent(new Event("resize"));
    if (typeof renderCorkStrings === "function") renderCorkStrings();
    if (typeof initCorkEvents === "function") initCorkEvents();
    if (typeof runFlapAnimation === "function") runFlapAnimation();
  }, 220);
});

$(document).on("shown.bs.tab", 'a[data-toggle="tab"], a[data-bs-toggle="tab"], a[data-bs-toggle="pill"]', function (e) {
  const activeTabTitle = $(e.target).find("span:first-child").text() || $(e.target).text();
  const indicator = document.getElementById("currentSectionBadge");
  if (indicator && activeTabTitle) {
    indicator.textContent = activeTabTitle.trim();
  }

  // Si no está fijado en desktop, cerrar el menú desplegable automáticamente
  if (!document.body.classList.contains("drawer-pinned")) {
    closeForensicDrawer();
  }

  setTimeout(() => {
    window.dispatchEvent(new Event("resize"));
    if (typeof renderCorkStrings === "function") renderCorkStrings();
    if (typeof initCorkEvents === "function") initCorkEvents();
    if (typeof runFlapAnimation === "function") runFlapAnimation();
  }, 220);
});
