theories/Base.vo theories/Base.glob theories/Base.v.beautified theories/Base.required_vo: theories/Base.v 
theories/Base.vio: theories/Base.v 
theories/Base.vos theories/Base.vok theories/Base.required_vos: theories/Base.v 
theories/RangeChecker.vo theories/RangeChecker.glob theories/RangeChecker.v.beautified theories/RangeChecker.required_vo: theories/RangeChecker.v theories/Base.vo
theories/RangeChecker.vio: theories/RangeChecker.v theories/Base.vio
theories/RangeChecker.vos theories/RangeChecker.vok theories/RangeChecker.required_vos: theories/RangeChecker.v theories/Base.vos
theories/PointwiseContradictions.vo theories/PointwiseContradictions.glob theories/PointwiseContradictions.v.beautified theories/PointwiseContradictions.required_vo: theories/PointwiseContradictions.v theories/Base.vo
theories/PointwiseContradictions.vio: theories/PointwiseContradictions.v theories/Base.vio
theories/PointwiseContradictions.vos theories/PointwiseContradictions.vok theories/PointwiseContradictions.required_vos: theories/PointwiseContradictions.v theories/Base.vos
theories/ClassBalance.vo theories/ClassBalance.glob theories/ClassBalance.v.beautified theories/ClassBalance.required_vo: theories/ClassBalance.v theories/Base.vo
theories/ClassBalance.vio: theories/ClassBalance.v theories/Base.vio
theories/ClassBalance.vos theories/ClassBalance.vok theories/ClassBalance.required_vos: theories/ClassBalance.v theories/Base.vos
theories/Checker.vo theories/Checker.glob theories/Checker.v.beautified theories/Checker.required_vo: theories/Checker.v theories/Base.vo theories/RangeChecker.vo theories/PointwiseContradictions.vo
theories/Checker.vio: theories/Checker.v theories/Base.vio theories/RangeChecker.vio theories/PointwiseContradictions.vio
theories/Checker.vos theories/Checker.vok theories/Checker.required_vos: theories/Checker.v theories/Base.vos theories/RangeChecker.vos theories/PointwiseContradictions.vos
extraction/Extract.vo extraction/Extract.glob extraction/Extract.v.beautified extraction/Extract.required_vo: extraction/Extract.v theories/Base.vo theories/RangeChecker.vo theories/PointwiseContradictions.vo theories/ClassBalance.vo theories/Checker.vo
extraction/Extract.vio: extraction/Extract.v theories/Base.vio theories/RangeChecker.vio theories/PointwiseContradictions.vio theories/ClassBalance.vio theories/Checker.vio
extraction/Extract.vos extraction/Extract.vok extraction/Extract.required_vos: extraction/Extract.v theories/Base.vos theories/RangeChecker.vos theories/PointwiseContradictions.vos theories/ClassBalance.vos theories/Checker.vos
