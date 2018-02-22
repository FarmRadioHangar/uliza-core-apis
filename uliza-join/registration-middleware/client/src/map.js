import React from 'react';
import { connect } from 'react-redux';
import GoogleMapReact from 'google-map-react';
import MarkerImage from '../img/pin.png';

class Marker extends React.Component {
  render() {
    return (
      <div style={{marginTop: '-24px', marginLeft: '-12px'}}>
        <img style={{width: '24px', height: '24px'}} src={MarkerImage} />
      </div>
    );
  }
}

const Map = ({ onClick, position, center }) => {
  return (
    <div style={{width: '100%', height: '400px', margin: '1em 0'}}>
      <GoogleMapReact
        onClick={e => { onClick(e); }}
        bootstrapURLKeys={{
          key: 'AIzaSyB7OueSg9kajTllsy-TrVALIcPqU57esLc'
        }}
        center={center}
        zoom={6}>
        {position && (
          <Marker 
            lat={position.lat} 
            lng={position.lng} />
        )}
      </GoogleMapReact>
    </div>
  );
}

const mapStateToProps = state => {
  return {
    position: state.location.position,
    center: state.map.center
  };
}

const mapDispatchToProps = dispatch => {
  return {
    onClick: pos => {
      dispatch({
        type: 'SET_LOCATION',
        ...pos
      });
    }
  };
}

const Component = connect(
  mapStateToProps,
  mapDispatchToProps
)(Map);

export default Component;
