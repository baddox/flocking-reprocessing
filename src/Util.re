open Reprocessing;

let wrap = (value, maxValue) =>
  value > maxValue ? 0.0 : value < 0.0 ? maxValue : value;

let random = (min, max) => Utils.randomf(~min, ~max);