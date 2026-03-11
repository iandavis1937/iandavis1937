

window.addEventListener('scroll', () => {
  const scrollPosition = window.scrollY;
  const windowHeight = window.innerHeight;
  const totalHeight = document.body.scrollHeight - windowHeight;
  const scrollPercentage = scrollPosition / totalHeight;
  const angle = 170 + (scrollPercentage * 15); // Adjust the 180 value to control how much the angle changes
  const blue = "#3a8fd9"; 
  const purple = "#686dc3"; 
  const teal2 = "#2bb7a2";
  document.body.style.backgroundImage = `linear-gradient(${angle}deg, ${blue}, ${purple}, ${teal2})`;
});