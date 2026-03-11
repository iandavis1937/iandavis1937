

const words = ["data scientist", "data storyteller", "survey designer", "social science researcher"];
let currentIndex = 0;

function flipWord() {
  currentIndex = (currentIndex + 1) % words.length;
  document.getElementById('flipWord').textContent = words[currentIndex];
}

// First call after 3300ms to account for time flipping
setTimeout(() => {
  flipWord();
  // Subsequent calls every 3000ms
  setInterval(flipWord, 3000);
}, 3300);