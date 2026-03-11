

window.addEventListener('scroll', () => {
  const scrollPosition = window.scrollY;
  const windowHeight = window.innerHeight;
  const totalHeight = document.body.scrollHeight - windowHeight;
  const scrollPercentage = scrollPosition / totalHeight;
  const angle = 170 + (scrollPercentage * 15); // Adjust the 180 value to control how much the angle changes
  const blue = "#518dc2";
  const purple = "#7276b8";
  const teal = "#6baaa0";
  document.body.style.backgroundImage = `linear-gradient(${angle}deg, ${blue} 35%, ${purple} 73%, ${teal} 99%)`;
});