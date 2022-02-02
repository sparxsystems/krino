using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// The adverbs interpreted as "quantitative" include words of degree.
    /// </summary>
    /// <remarks>
    /// https://studfile.net/preview/4083865/page:23/
    /// hese are specific lexical units of semi-functional nature expressing quality measure, or gradational evaluation of qualities.
    /// </remarks>
    public class AdverbDegreeSememes : EnumGroupBase
    {
        public AdverbDegreeSememes(EnumGroupBase parent) : base(parent)
        {
            HighDegree = new EnumValue(this);
            ExcessiveDegree = new EnumValue(this);
            UnexpectedDegree = new EnumValue(this);
            ModerateDegree = new EnumValue(this);
            LowDegree = new EnumValue(this);
            ApproximateDegree = new EnumValue(this);
            OptimalDegree = new EnumValue(this);
            InadequateDegree = new EnumValue(this);
            UnderDegree = new EnumValue(this);
        }

        /// <summary>
        /// E.g. very, quite, entirely, utterly, highly, greatly, perfectly, ...
        /// </summary>
        /// <remarks>
        /// These adverbs are sometimes classed as "intensifiers".
        /// </remarks>
        public EnumValue HighDegree { get; }

        /// <summary>
        /// E.g. too, awfully, tremendously, dreadfully, terrifically.
        /// </summary>
        /// <remarks>
        /// As HighDegree they also belong to the broader subclass of intensifiers.
        /// </remarks>
        public EnumValue ExcessiveDegree { get; }

        /// <summary>
        /// E.g. surprisingly, astonishingly, amazingly
        /// </summary>
        public EnumValue UnexpectedDegree { get; }

        /// <summary>
        /// E.g. fairly, comparatively, relatively, moderately, rather
        /// </summary>
        public EnumValue ModerateDegree { get; }

        /// <summary>
        /// E.g. slightly, a little, a bit.
        /// </summary>
        public EnumValue LowDegree { get; }

        /// <summary>
        /// E.g. almost, nearly.
        /// </summary>
        public EnumValue ApproximateDegree { get; }

        /// <summary>
        /// E.g. enough, sufficiently, adequately.
        /// </summary>
        public EnumValue OptimalDegree { get; }

        /// <summary>
        /// E.g. insufficiently, intolerably, unbearably, ridiculously.
        /// </summary>
        public EnumValue InadequateDegree { get; }

        /// <summary>
        /// E.g. hardly, scarcely.
        /// </summary>
        public EnumValue UnderDegree { get; }
    }
}
