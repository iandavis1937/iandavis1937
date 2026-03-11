

function logWebpageWidth() {
  const webpageWidth = window.innerWidth;
  console.log('Webpage width:', webpageWidth, 'px');
}

// Log the width when the page loads
window.onload = logWebpageWidth;

// Log the width when the window is resized
window.onresize = logWebpageWidth;