/* eslint-disable */

async function day6() {
  const file = (await Bun.file('../inputs/2024/Day6/input.txt').text()).trim();
  const lines = file.split('\n');

  console.log('🌟 --- Day 6 Results --- 🌟');
  console.time('Total Time');
  console.time('T1');
  console.log('📌 Part 1: ', part1(lines));
  console.timeEnd('T1');
  console.time('T2');
  console.log('📌 Part 2: ', part2(lines));
  console.timeEnd('T2');
  console.timeEnd('Total Time');
}
/**
 *
 * @param {String[]} lines
 * @returns
 */
function part1(lines) {
  // convert input into matrix
  const matrix = lines.map(row => row.split(''));
  // get starting guard coordinates
  const startingCoords = findCoords(matrix, '^');
  // create a guard
  const guard = new Guard(startingCoords.x, startingCoords.y, 'UP');
  // mark starting spot
  matrix[guard.y][guard.x] = 'X';

  while (true) {
    // get coordinates for next guard position
    const { x, y } = guard.nextPosition();
    // check if next position is out of bounds
    if (x < 0 || x > matrix.length - 1 || y < 0 || y > matrix.length - 1) {
      break;
    }
    const nextPosition = matrix[y][x];
    // move the guard to the next position
    guard.move(nextPosition);
    // do a 180 if blocked in 2 ways
    if (matrix[guard.y][guard.x] === '#') {
      guard.x = x;
      guard.y = y;
      guard.move('#');
    }
    // mark the spot
    matrix[guard.y][guard.x] = 'X';
  }
  // count all X
  return matrix.reduce((total, row) => total + row.reduce((count, char) => (char === 'X' ? count + 1 : count), 0), 0);
}

function part2(lines) {
  let obstacles = 0;
  // convert input into matrix
  const matrix = lines.map(row => row.split(''));
  // get starting guard coordinates
  const startingCoords = findCoords(matrix, '^');
  // create a guard
  const guard = new Guard(startingCoords.x, startingCoords.y, 'UP');

  matrix.forEach(row =>
    row.forEach((char, i) => {
      // reset guard position
      guard.reset();

      if (char === '.') {
        row[i] = 'O';

        while (true) {
          // get coordinates for next guard position
          const { x, y } = guard.nextPosition();
          // check if next position is out of bounds
          if (x < 0 || x > matrix.length - 1 || y < 0 || y > matrix.length - 1) {
            row[i] = '.';

            break;
          }
          if (guard.insideLoop()) {
            obstacles++;
            row[i] = '.';
            break;
          }
          const nextPosition = matrix[y][x];
          // move the guard to the next position
          guard.move(nextPosition);

          // do a 180 if blocked in 2 ways
          if (matrix[guard.y][guard.x] === '#' || matrix[guard.y][guard.x] === 'O') {
            guard.x = x;
            guard.y = y;
            guard.move('#');
          }
        }
      }
    }),
  );

  return obstacles;
}

day6();

/**
 * Function to return {x,y} coordinates of a value in a matrix
 * @param {String[]} matrix
 * @param {String} value
 * @returns
 */
function findCoords(matrix, value) {
  for (let y = 0; y < matrix.length; y++) {
    const x = matrix[y].indexOf(value);
    if (x !== -1) {
      return { x, y };
    }
  }
  return null;
}

class Guard {
  constructor(x, y, direction) {
    this.xi = x;
    this.yi = y;
    this.directioni = direction;
    this.x = x;
    this.y = y;
    this.direction = direction;
    this.history = new Map();
    this.steps = 0;
  }
  reset() {
    this.x = this.xi;
    this.y = this.yi;
    this.direction = this.directioni;
    this.history = new Map();
    this.steps = 0;
  }
  insideLoop() {
    return this.steps > this.history.size;
  }
  move(char) {
    // before moving check if there's an obstacle and change direction
    this.checkObstacle(char);
    switch (this.direction) {
      case 'UP':
        this.y--;
        break;
      case 'DOWN':
        this.y++;
        break;
      case 'LEFT':
        this.x--;
        break;
      case 'RIGHT':
        this.x++;
        break;
    }
    this.steps++;
    this.history.set(JSON.stringify({ direction: this.direction, x: this.x, y: this.y }));
  }
  checkObstacle(char) {
    if (char === '#' || char == 'O') {
      switch (this.direction) {
        case 'UP':
          this.direction = 'RIGHT';
          break;
        case 'DOWN':
          this.direction = 'LEFT';
          break;
        case 'LEFT':
          this.direction = 'UP';
          break;
        case 'RIGHT':
          this.direction = 'DOWN';
          break;
      }
    }
  }
  nextPosition() {
    switch (this.direction) {
      case 'UP':
        return { x: this.x, y: this.y - 1 };
      case 'DOWN':
        return { x: this.x, y: this.y + 1 };
      case 'LEFT':
        return { x: this.x - 1, y: this.y };
      case 'RIGHT':
        return { x: this.x + 1, y: this.y };
    }
  }
}
