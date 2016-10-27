module.exports = function() {
  const state = {
    last: 0, // seconds
    freq: 0, // cps
    phase: 0 // [0,1]
  };
  Math.fmod = (a,b) => 
    Number((a - (Math.floor(a / b) * b)).toPrecision(8));

  this.emitBeat = () => {
    const now = (new Date())/1000; // in seconds
    const diff = now - state.last; // in seconds
    state.last = now;
    state.freq = 1/diff;

    const phaseDiff = state.phase - 0; // we expect phase to be zero
    state.phase = Math.fmod(state.phase - phaseDiff, 1);
  };

  // getPhase. To be called once over 1/srate.
  this.getPhase = (srate) => {
    state.phase = (state.phase + state.freq*(1/srate))% 1;
    return state.phase;
  };

};
