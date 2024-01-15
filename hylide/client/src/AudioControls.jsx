import React, {PropTypes} from "react";
import ReactDOM from "react-dom";

import {PlayButton, Progress} from 'react-soundplayer/components';
import Audio from "./Audio";



const SC = React.createClass({
  propTypes: {
    initPlaying: PropTypes.bool
  },
  getDefaultProps() {
    return {
      initPlaying: false
    };
  },
  getInitialState() {
    /* console.log(localStorage.getItem("scurl")); */
    let defaulturl = localStorage.getItem("scurl")
                  || "https://soundcloud.com/tennysonmusic/xyz"
                  || "https://soundcloud.com/wearesilkenwood/klypht-spiraling-chasm"
                  || "https://soundcloud.com/tennysonmusic/angus-julia-stone-for-you"
                  || "https://soundcloud.com/herzeloyde/deception"
                  || "https://soundcloud.com/aslamin/strannoe-chuvstvo";
    return {
      loading: false,
      playing: this.props.initPlaying,
      val: 0,
      url: defaulturl
    };
  },
  async startPlaying() {
    if (!Audio.scPlayer) {
        this.setState({ loading: true });
        await Audio.initializeAudioSoundCloud(this.state.url, this.props.initPlaying);
        Audio.scPlayer.play();
        // Hack to get around some weirdness with the soundcloud player:
        await new Promise((resolve) => setTimeout(resolve, 2000));
        Audio.scPlayer.pause();
        await new Promise((resolve) => setTimeout(resolve, 2000));
        this.setState({ loading: false });
    }
    this.startUpdating();
    Audio.scPlayer.play();
  },
  stopPlaying() {
    /* console.log("clearing ", this.intervalId); */
    window.clearTimeout(this.intervalId);
    Audio.scPlayer.pause();
  },
  componentDidMount() {
    if (this.props.initPlaying) {
      this.startUpdating();
    }
  },
  componentWillUpdate(nprops, nstate) {
    if (nstate.playing !== this.state.playing){
      if (nstate.playing) {
        this.startPlaying();
      } else {
        this.stopPlaying();
      }
    }
  },
  togglePlay() {
    this.setState((prev) => {return {playing: !prev.playing};});
  },
  update() {
    let val = Audio.scPlayer.audio.currentTime/(Audio.scPlayer.duration / 100);
    this.setState({val: val});
  },
  startUpdating() {
    this.intervalId = window.setInterval(this.update, 100);
    /* console.log("made new interval:", this.intervalId); */
  },
  componentWillUnmount() {
    /* console.log("clearing: ", this.intervalId); */
    window.clearTimeout(this.intervalId);
  },
  onUrlChange(e) {
    this.setState({url: e.currentTarget.value});
  },
  onSubmit(e) {
    e.preventDefault();
    localStorage.setItem("scurl", this.state.url);
    Audio.scPlayer.resolve(this.state.url, function(track) {
      /* console.log(track); */
      this.setState({playing: false}, function() {
        /* console.log("callback"); */
        this.setState({playing: true});
      }.bind(this));
    }.bind(this));
  },
  render: function() {
    let buttonVal = this.state.playing ? "[pause]" : "[play]";
    if (this.state.loading) {
      buttonVal = "[loading...]";
    }
    return (
        <div className="scPlayer">
        <div className="row">
            <button onClick={this.togglePlay}
            style={{ cursor: 'pointer' }}
            enabled={this.state.loading ? "false" : "true"}
            > {buttonVal}</button>
            <Progress innerStyle={{}}
                      soundCloudAudio={Audio.scPlayer}
                      value={this.state.val}/>
          </div>
          <div className="row">
            <form onSubmit={this.onSubmit}>
              <input value={this.state.url}
                     style={{width: "40vw"}}
                    onChange={this.onUrlChange}/>
            </form>
          </div>
        </div>
    );
  }
});

const UserMedia = React.createClass({
  componentWillMount() {
    Audio.initializeAudioUserMedia();
  },
  render: function() {
    return (
      <div>
      </div>
    );
  }
});

export default React.createClass({
  propTypes: {
    initialState: PropTypes.string
  },
  getDefaultProps() {
    return {
      initialState: "sc"
    };
  },
  getInitialState: function() {
    return {
      state: this.props.initialState,
      initPlaying: false
    };
  },
  onChange(e) {
    this.setState({
      state: e.currentTarget.value,
      initPlaying: false
    });
  },
  render() {

    let component = this.state.state === "sc"
                  ? <SC initPlaying={this.state.initPlaying}/>
                  : <UserMedia/>;


    return (
      <div className="audioControls">
        <div className="inputPicker"></div>
        {/* <div className="inputPicker">
          <br/>
          sc : <input type="radio"
                 checked={this.state.state === "sc"}
                 value="sc"
                 onChange={this.onChange}/>
          <br/>
          mic: <input type="radio"
                 checked={this.state.state === "usermedia"}
                 value="usermedia"
                 onChange={this.onChange}/>
        </div> */}
        {component}
      </div>
    );
  }
});
