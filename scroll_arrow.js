

document.addEventListener('DOMContentLoaded', () => {
  // Select the search button and the 'check this out image'
  const searchButtonIcon = document.querySelector('.aa-DetachedSearchButtonIcon');
  const imageElement = document.querySelector('img[src="images/check_this_out.png"]');

  // Ensure the elements exist
  if (searchButtonIcon) {
    // Get the bounding rectangle of the element
    const rect = searchButtonIcon.getBoundingClientRect();
    window.searchButtonX = rect.left;
    console.log('X position:', searchButtonX, 'px');
  } else {
    console.error('Element .aa-DetachedSearchButtonIcon not found');
  }

  if (imageElement) {
    // Get the bounding rectangle of the element
    const rect = imageElement.getBoundingClientRect();

    // Calculate the right border position
    window.rightBorderPosition = rect.left + rect.width;
  }

  // Set the initial width of the horizontal line
  const horizontalLine = document.querySelector('.horizontal-line');

  window.minLength = 100; // Minimum width for the horizontal line
  horizontalLine.style.width = `${minLength}px`;
});

window.addEventListener('scroll', () => {
  const scrollPosition = window.scrollY;
  const windowHeight = window.innerHeight;
  const totalHeight = document.body.scrollHeight - windowHeight;
  const scrollPercentage = scrollPosition / totalHeight;
  console.log('Scroll Percentage:', scrollPercentage);

  const xLength = window.searchButtonX - window.rightBorderPosition;
  console.log('xLength:', xLength);

  const horizontalLine = document.querySelector('.horizontal-line');
  const verticalLine = document.querySelector('.vertical-line');
  const maxHeight = 1000; // Maximum height the arrow can reach vertically

  if (scrollPercentage <= 0.5) {
    // Adjust the width of the horizontal line based on scroll percentage
    const newWidth = window.minLength + (xLength * (scrollPercentage * 2));
    console.log('newWidth:', newWidth);
    horizontalLine.style.width = `${newWidth}px`;
    horizontalLine.classList.remove('hide-arrowhead');
    verticalLine.style.display = 'none';

  } else if (scrollPercentage > 0.5) {
    // Adjust the height of the vertical line based on scroll percentage
    const horizontalEnd = window.minLength + xLength;
    const newHeight = maxHeight * ((scrollPercentage - 0.5) * 2); // Adjust scroll percentage for vertical movement
    horizontalLine.style.width = `${horizontalEnd}px`;
    verticalLine.style.display = 'block';
    verticalLine.style.left = `${horizontalEnd - 82}px`; // Position the vertical line where the horizontal line ends
    verticalLine.style.height = `${newHeight}px`;
    horizontalLine.classList.add('hide-arrowhead');
  }
});
