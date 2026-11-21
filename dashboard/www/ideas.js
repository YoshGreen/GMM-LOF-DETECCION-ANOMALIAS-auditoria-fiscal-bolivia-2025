// =============================================================================
// IDEAS.JS — COMPONENTES VISUALES INTERACTIVOS (Ideas/)
// Corcho Criminalístico, Tablero Split-Flap y Cinta de Sumadora
// =============================================================================

// 1. CORCHO CRIMINALÍSTICO (Hilos de conexión roja entre municipios)
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
      const y1 = p1.top - rect.top; // Desde la chincheta superior
      const x2 = p2.left + p2.width / 2 - rect.left;
      const y2 = p2.top - rect.top;
      
      const sag = Math.max(y1, y2) + 40; // Curvatura natural del hilo colgante
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

// 2. TABLERO SPLIT-FLAP (Telemetría Aeroportuaria / Terminal Forense)
const FLAP_CHARS = "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ0123456789.%- ";
function setFlapChar(el, targetChar, steps) {
  let count = 0;
  const interval = setInterval(() => {
    if (++count >= steps) {
      el.textContent = targetChar;
      clearInterval(interval);
    } else {
      el.textContent = FLAP_CHARS[Math.floor(Math.random() * FLAP_CHARS.length)];
    }
  }, 40);
}

function runFlapAnimation() {
  const rows = document.querySelectorAll(".flap-row");
  rows.forEach((row, rowIndex) => {
    const targetText = row.getAttribute("data-flat") || "";
    const cells = row.querySelectorAll(".flap-cell");
    cells.forEach((cell, cellIndex) => {
      const targetChar = targetText[cellIndex] || " ";
      setFlapChar(cell, targetChar, 5 + (cellIndex % 8) * 2 + rowIndex * 2);
    });
  });
}

// 3. CINTA DE SUMADORA CONTABLE
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
    { type: "small", text: "EJEC 31,9% · DEUDA 16,2% · PARÁLISIS" },
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
    await new Promise(r => setTimeout(r, item.type === "dashed" ? 30 : 65));
  }
}

// Disparadores automáticos al cargar y cambiar de pestaña
window.addEventListener("resize", () => {
  renderCorkStrings();
});

document.addEventListener("DOMContentLoaded", () => {
  setTimeout(() => {
    renderCorkStrings();
    initCorkEvents();
    runFlapAnimation();
    printReceiptSequence();
  }, 600);
});

// Listener para cuando Shiny cambia de tab
$(document).on("shown.bs.tab", 'a[data-bs-toggle="pill"]', function (e) {
  setTimeout(() => {
    renderCorkStrings();
    initCorkEvents();
    runFlapAnimation();
  }, 250);
});
