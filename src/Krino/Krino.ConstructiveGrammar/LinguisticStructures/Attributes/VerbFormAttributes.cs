using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class VerbFormAttributes : EnumGroupBase
    {
        public VerbFormAttributes(EnumGroupBase parent) : base(parent)
        {
            Base = new VerbNumberForm(this);
            Past = new VerbNumberForm(this);
            PastParticiple = new EnumValue(this);
            Ing = new EnumValue(this);
            PresentParticiple = new EnumValue(this);
            Gerund = new EnumValue(this);
        }

        /// <summary>
        /// The version of the verb without any endings.
        /// </summary>
        public VerbNumberForm Base { get; }

        /// <summary>
        /// Past form of a verb.
        /// </summary>
        public VerbNumberForm Past { get; }

        /// <summary>
        /// A verb form which is used in forming perfect and passive tenses and sometimes as an adjective.
        /// (Third form of the verb.)
        /// </summary>
        public EnumValue PastParticiple { get; }

        /// <summary>
        /// ???
        /// Ing form of the verb acting as gerund or present participle.
        /// </summary>
        public EnumValue Ing { get; }

        /// <summary>
        /// The present participle verb form is created by adding -ing to the root word.
        /// It can also act as an adjective.
        /// </summary>
        /// <remarks>
        /// The present participle verb form is created by adding -ing to the root word.
        /// It’s used in the past, present, and future progressive verb tenses. 
        /// </remarks>
        public EnumValue PresentParticiple { get; }
        

        /// <summary>
        /// ???
        /// A verb form which acts as a noun. In English ending with -ing.
        /// </summary>
        /// <remarks>
        /// Gerunds are same as PresentParticiple - it differs only in their using.
        /// Gerunds are used as nouns.
        /// </remarks>
        public EnumValue Gerund { get; }
    }
}
