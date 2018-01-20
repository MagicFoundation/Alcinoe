//fake module

exports.createHash = exports.Hash = Hash;
function Hash(algorithm, options) {
  if (!(this instanceof Hash))
    return new Hash(algorithm, options);
/*  this._binding = new binding.Hash(algorithm);
  LazyTransform.call(this, options);*/
  this.fake = true;
}

exports.randomBytes = randomBytes;
function randomBytes(size, callback) {
return 'zzzzz';
}

