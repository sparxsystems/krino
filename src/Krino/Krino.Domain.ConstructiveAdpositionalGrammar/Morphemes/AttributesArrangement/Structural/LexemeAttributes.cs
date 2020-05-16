using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for lexemes.
    /// </summary>
    public class LexemeAttributes : EnumGroupBase
    {
        public LexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            O = new StativeAttributes(this);
            I = new VerbantAttributes(this);
            A = new AdjunctiveAttributes(this);
            E = new CircumstantialAttributes(this);
            U = new AdPositionAttributes(this);
        }

        /// <summary>
        /// Stative
        /// </summary>
        public StativeAttributes O { get; }

        /// <summary>
        /// Verbant
        /// </summary>
        public VerbantAttributes I { get; }

        /// <summary>
        /// Adjunctive
        /// </summary>
        public AdjunctiveAttributes A { get; }

        /// <summary>
        /// Circumstantial
        /// </summary>
        public CircumstantialAttributes E { get; }

        /// <summary>
        /// AdPosition
        /// </summary>
        public AdPositionAttributes U { get; }
    }
}
