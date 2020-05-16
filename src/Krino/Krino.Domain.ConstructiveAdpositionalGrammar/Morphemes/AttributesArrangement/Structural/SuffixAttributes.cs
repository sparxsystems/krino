using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for suffixes.
    /// </summary>
    public class SuffixAttributes : EnumGroupBase
    {
        public SuffixAttributes(EnumGroupBase parent) : base(parent)
        {
            O = new StativeAttributes(this);
            I = new VerbantAttributes(this);
            A = new AdjunctiveAttributes(this);
            E = new CircumstantialAttributes(this);
        }

        /// <summary>
        /// Suffix creates stative
        /// </summary>
        public StativeAttributes O { get; }

        /// <summary>
        /// Suffix creates verbant
        /// </summary>
        public VerbantAttributes I { get; }

        /// <summary>
        /// Suffix creates adjunctive
        /// </summary>
        public AdjunctiveAttributes A { get; }

        /// <summary>
        /// Suffix creates circumstantial
        /// </summary>
        public CircumstantialAttributes E { get; }
    }
}
