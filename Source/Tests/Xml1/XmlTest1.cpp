/*****************************************************************
|
|      XML Test Program 1
|
|      (c) 2001-2003 Gilles Boccon-Gibod
|      Author: Gilles Boccon-Gibod (bok@bok.net)
|
 ****************************************************************/

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#include "Neptune.h"
#include "NptDebug.h"

/*----------------------------------------------------------------------
|       CHECK
+---------------------------------------------------------------------*/
#define CHECK(test, message)                                            \
do {                                                                    \
    if (!(test)) {                                                      \
        fprintf(stderr, "FAILED: (%s) line %d\n", (const char*)(message), __LINE__); \
        NPT_ASSERT(0);                                                  \
    }                                                                   \
} while(0)

#ifdef TEST_WRITER
/*----------------------------------------------------------------------
|       WriterTest1
+---------------------------------------------------------------------*/
static void
WriterTest1()
{
    NPT_XmlElementNode* top = new NPT_XmlElementNode("top");
    NPT_XmlElementNode* child1 = new NPT_XmlElementNode("child1");
    child1->AddAttribute("someAttribute", "someValue");
    top->AddChild(child1);
    NPT_XmlElementNode* child2 = new NPT_XmlElementNode("child2");
    child2->AddAttribute("someOtherAttribute", "someOtherValue");
    child2->AddText("Some Text");
    child1->AddChild(child2);
    NPT_XmlElementNode* child3 = new NPT_XmlElementNode("child3");
    child3->AddAttribute("thirdArrtibute", "3");
    child2->AddChild(child3);
    
    NPT_XmlWriter writer;
    NPT_File out(NPT_FILE_STANDARD_OUTPUT);
    out.Open(NPT_FILE_OPEN_MODE_WRITE);
    NPT_OutputStreamReference out_stream;
    out.GetOutputStream(out_stream);
    
    writer.Serialize(*top, *out_stream);
}
#endif

#if defined(_WIN32) && defined(_DEBUG)
#include <crtdbg.h>
#endif

/*----------------------------------------------------------------------
|       TestNamespaces
+---------------------------------------------------------------------*/
static void
TestNamespaces()
{
    NPT_XmlElementNode* top = new NPT_XmlElementNode("top");
    top->SetNamespaceUri("", "http://namespace1.com");
    CHECK(top->GetNamespaceUri("") &&
        *(top->GetNamespaceUri("")) == "http://namespace1.com", "");

    NPT_XmlElementNode* child1 = new NPT_XmlElementNode("child1");
    top->AddChild(child1);
    CHECK(child1->GetNamespaceUri(""), "");
    CHECK(*(child1->GetNamespaceUri("")) == "http://namespace1.com", "");

    NPT_XmlElementNode* child2 = new NPT_XmlElementNode("ns1", "child2");
    top->AddChild(child2);
    CHECK(child2->GetNamespaceUri(""), "");
    CHECK(*(child2->GetNamespaceUri("")) == "http://namespace1.com", "");
    CHECK(child2->GetNamespaceUri("ns1") == NULL, "");
    child2->SetNamespaceUri("ns1", "http://blabla");
    CHECK(child2->GetNamespaceUri("ns1"), "");
    CHECK(*child2->GetNamespaceUri("ns1") == "http://blabla", "");
    CHECK(*child2->GetNamespace() == "http://blabla", "");

    // testing a child with a namespace defined in parent
    NPT_XmlElementNode* child3 = new NPT_XmlElementNode("ns1", "child3");
    child2->AddChild(child3);
    CHECK(child3->GetNamespaceUri(""), "");
    CHECK(*(child3->GetNamespaceUri("")) == "http://namespace1.com", "");
    CHECK(child3->GetNamespaceUri("ns1"), "");
    CHECK(*child3->GetNamespaceUri("ns1") == "http://blabla", "");
    CHECK(*child3->GetNamespace() == "http://blabla", "");

    // testing adding a namespace in a node which namespace is defined in parent
    child3->SetNamespaceUri("ns3", "http://foofoo");
    CHECK(child3->GetNamespaceUri("ns1"), "");
    CHECK(*child3->GetNamespaceUri("ns1") == "http://blabla", "");
    CHECK(*child3->GetNamespace() == "http://blabla", "");

    const char* xml1 = 
        "<top>"
        "  <child1 xmlns:foo='blabla'><cc1 foo:attr1='0'/></child1>"
        "  <child2 xmlns='foobar' attr1='1'>"
        "    <cc2/>"
        "    <cc3 />"
        "  </child2 >"
        "  <ns2:child3 xmlns:ns2='abcd'><cc3/></ns2:child3>"
        "  <child4 ns3:attr1='3' xmlns:ns3='efgh'>"
        "    <ns3:cc4 ns3:attr1='4'/>"
        "  </child4>"
        "</top>";
    NPT_XmlParser parser;
    NPT_XmlNode* root = NULL;
    NPT_Result result = parser.Parse(xml1, root);
    CHECK(NPT_SUCCEEDED(result), "");
    CHECK(root != NULL, "");

    NPT_XmlWriter    writer;
    NPT_MemoryStream output;
    writer.Serialize(*root, output);
    NPT_Size size;
    output.GetSize(size);
    printf(NPT_String((const char*)output.GetData(), size).GetChars());

    delete top;
    delete root;
}

/*----------------------------------------------------------------------
|       TestSerializer
+---------------------------------------------------------------------*/
static void
TestSerializer()
{
    NPT_XmlWriter    writer;
    NPT_MemoryStream output;
    NPT_String       check;
    NPT_Size         size;

    //
    // test without namespaces
    //

    // simple element with no prefix and no namespace
    NPT_XmlElementNode* top = new NPT_XmlElementNode("top");
    writer.Serialize(*top, output);
    output.GetSize(size);
    check.Assign((const char*)output.GetData(), size);
    CHECK(check == "<top/>", check);

    // with one attribute
    output.SetSize(0);
    top->SetAttribute("attr1", "b&w");
    writer.Serialize(*top, output);
    output.GetSize(size);
    check.Assign((const char*)output.GetData(), size);
    CHECK(check == "<top attr1=\"b&amp;w\"/>", check);

    // add one child
    output.SetSize(0);
    delete top;
    top = new NPT_XmlElementNode("top");
    NPT_XmlElementNode* child1 = new NPT_XmlElementNode("child1");
    top->AddChild(child1);
    writer.Serialize(*top, output);
    output.GetSize(size);
    check.Assign((const char*)output.GetData(), size);
    CHECK(check == "<top><child1/></top>", check);

    //
    // test with namespaces
    //

    // test default namespaces
    output.SetSize(0);
    delete top;
    top = new NPT_XmlElementNode("top");
    top->SetNamespaceUri("", "http://namespace.com");
    writer.Serialize(*top, output);
    output.GetSize(size);
    check.Assign((const char*)output.GetData(), size);
    CHECK(check == "<top xmlns=\"http://namespace.com\"/>", check);

    // test attribute prefixes
    output.SetSize(0);
    delete top;
    top = new NPT_XmlElementNode("top");
    top->SetAttribute(NULL,  "foo", "6");
    top->SetAttribute("ns1", "foo", "3");
    top->SetAttribute("ns2", "foo", "4");
    top->SetAttribute("ns1", "foo", "5");
    writer.Serialize(*top, output);
    output.GetSize(size);
    check.Assign((const char*)output.GetData(), size);
    CHECK(check == "<top foo=\"6\" ns1:foo=\"5\" ns2:foo=\"4\"/>", check);

    delete top;
}

/*----------------------------------------------------------------------
|       TestCanonicalizer
+---------------------------------------------------------------------*/
static void
TestCanonicalizer()
{
    extern const char* xml_cano_1[];

    NPT_XmlParser parser(true); // do not ignore whitespaces
    NPT_XmlNode* root;

    for (unsigned int i=0; xml_cano_1[i]; i+=2) {
        const char* xml_in = xml_cano_1[i];
        const char* xml_out = xml_cano_1[i+1];
        NPT_ASSERT(NPT_SUCCEEDED(parser.Parse(xml_in, root)));
        NPT_ASSERT(root);

        NPT_XmlCanonicalizer canonicalizer;
        NPT_MemoryStream buffer1;
        NPT_Result result = canonicalizer.Serialize(*root, buffer1);

        NPT_String str((const char*)buffer1.GetData(), buffer1.GetDataSize());
        NPT_Debug("%s", str.GetChars());
        NPT_ASSERT(str == xml_out);

        delete root;

        NPT_ASSERT(NPT_SUCCEEDED(parser.Parse(str, root)));
        NPT_ASSERT(root);
        NPT_MemoryStream buffer2;
        result = canonicalizer.Serialize(*root, buffer2);
        NPT_ASSERT(buffer1.GetBuffer() == buffer2.GetBuffer());

        delete root;
    }
}

/*----------------------------------------------------------------------
|       TestRegression
+---------------------------------------------------------------------*/
static void
TestRegression()
{
    // test for a bug found when the XML parser would try
    // to compare a null prefix
    NPT_XmlElementNode* element = new NPT_XmlElementNode("hello");
    element->SetAttribute("ns", "foo", "6");
    element->SetAttribute("foo", "5");
    element->SetAttribute("ns", "foo", "7");
    element->SetAttribute("foo", "8");
    NPT_ASSERT(*element->GetAttribute("foo") == "8");
    NPT_ASSERT(*element->GetAttribute("foo", "ns") == "7");
    
    delete element;
}

/*----------------------------------------------------------------------
|       TestWhitespace
+---------------------------------------------------------------------*/
static void
TestWhitespace()
{
    const char* xml = 
"<doc>\r\n"
"   <clean>   </clean>\r\n"
"   <dirty>   A   B   </dirty>\r\n"
"   <mixed>\r\n"
"      A\r\n"
"      <clean>   </clean>\r\n"
"      B\r\n"
"      <dirty>   A   B   </dirty>\r\n"
"      C\r\n"
"   </mixed>\r\n"
"</doc>\r\n";

    const char* expect1 = 
"<doc><clean/><dirty>   A   B   </dirty><mixed>\n"
"      A\n"
"      <clean/>\n"
"      B\n"
"      <dirty>   A   B   </dirty>\n"
"      C\n"
"   </mixed></doc>";

    const char* expect2 = 
"<doc>\n"
"   <clean>   </clean>\n"
"   <dirty>   A   B   </dirty>\n"
"   <mixed>\n"
"      A\n"
"      <clean>   </clean>\n"
"      B\n"
"      <dirty>   A   B   </dirty>\n"
"      C\n"
"   </mixed>\n"
"</doc>";

    NPT_XmlParser parser1; // ignore whitespace (default)
    NPT_XmlNode* root;
    NPT_ASSERT(NPT_SUCCEEDED(parser1.Parse(xml, root)));
    NPT_ASSERT(root);

    NPT_XmlWriter writer;
    NPT_MemoryStream buffer;
    writer.Serialize(*root, buffer);
    NPT_ASSERT(buffer.GetBuffer() == NPT_DataBuffer(expect1, NPT_StringLength(expect1)));

    delete root;

    NPT_XmlParser parser2(true); // keep whitespace
    NPT_ASSERT(NPT_SUCCEEDED(parser2.Parse(xml, root)));
    NPT_ASSERT(root);

    buffer.SetSize(0);
    writer.Serialize(*root, buffer);
    NPT_ASSERT(buffer.GetBuffer() == NPT_DataBuffer(expect2, NPT_StringLength(expect2)));

    delete root;
}

/*----------------------------------------------------------------------
|       TestComments
+---------------------------------------------------------------------*/
static void
TestComments()
{
    const char* xml = 
        "<!-- comment outside the element -->\n"
        "<doc> blabla <!-- --> foo <!-- you <g> &foo -> &bar --> blibli</doc>";
    const char* expect = "<doc> blabla  foo  blibli</doc>";

    NPT_XmlParser parser;
    NPT_XmlNode* root;
    NPT_ASSERT(NPT_SUCCEEDED(parser.Parse(xml, root)));
    NPT_ASSERT(root);

    NPT_XmlWriter writer;
    NPT_MemoryStream buffer;
    writer.Serialize(*root, buffer);
    NPT_ASSERT(buffer.GetBuffer() == NPT_DataBuffer(expect, NPT_StringLength(expect)));

    delete root;
}

/*----------------------------------------------------------------------
|       TestCdata
+---------------------------------------------------------------------*/
static void
TestCdata()
{
    const char* xml = 
        "<doc> blabla <![CDATA[  < [[  Smith ]] >   ]]> blibli</doc>";
    const char* expect = "<doc> blabla   &lt; [[  Smith ]] &gt;    blibli</doc>";

    NPT_XmlParser parser;
    NPT_XmlNode* root;
    NPT_ASSERT(NPT_SUCCEEDED(parser.Parse(xml, root)));
    NPT_ASSERT(root);

    NPT_XmlWriter writer;
    NPT_MemoryStream buffer;
    writer.Serialize(*root, buffer);
    NPT_ASSERT(buffer.GetBuffer() == NPT_DataBuffer(expect, NPT_StringLength(expect)));

    delete root;
}

/*----------------------------------------------------------------------
|       TestAttributes
+---------------------------------------------------------------------*/
static void
TestAttributes()
{
    const char* xml = 
        "<element foo='blabla'><cc1 attr1='0'/></element>";
    NPT_XmlParser parser;
    NPT_XmlNode* root = NULL;
    NPT_Result result = parser.Parse(xml, root);
    CHECK(NPT_SUCCEEDED(result), "");
    CHECK(root != NULL, "");
    CHECK(root->AsElementNode() != NULL, "");
    const NPT_String* a = root->AsElementNode()->GetAttribute("foo");
    CHECK(*a == "blabla", "");
    delete root;
}

/*----------------------------------------------------------------------
|       TestFile
+---------------------------------------------------------------------*/
static void
TestFile(const char* filename)
{
    NPT_File*                input;
    NPT_InputStreamReference stream;
    NPT_Result               result;

    // open the input file
    input = new NPT_File(filename);
    result = input->Open(NPT_FILE_OPEN_MODE_READ);
    if (NPT_FAILED(result)) {
        NPT_Debug("XmtTest1:: cannot open input (%d)\n", result);
        return;
    }
    result = input->GetInputStream(stream);

    // parse the buffer
    NPT_XmlParser parser;
    NPT_XmlNode*  tree;
    result = parser.Parse(*stream, tree);
    if (NPT_FAILED(result)) {
        NPT_Debug("XmlTest1:: cannot parse input (%d)\n", result);
        return;
    }


#ifdef TEST_WRITER
    // dump the tree
    NPT_XmlWriter writer(2);
    NPT_File output(NPT_FILE_STANDARD_OUTPUT);
    output.Open(NPT_FILE_OPEN_MODE_WRITE);
    NPT_OutputStreamReference output_stream_ref;
    output.GetOutputStream(output_stream_ref);
    writer.Serialize(*tree, *output_stream_ref);
#endif

    // delete the tree
    delete tree;

    // delete the input
    delete input;
}

/*----------------------------------------------------------------------
|       main
+---------------------------------------------------------------------*/
int
main(int argc, char** argv)
{
    // setup debugging
#if defined(_WIN32) && defined(_DEBUG)
    int flags = _crtDbgFlag       | 
        _CRTDBG_ALLOC_MEM_DF      |
        _CRTDBG_DELAY_FREE_MEM_DF |
        _CRTDBG_CHECK_ALWAYS_DF;

    _CrtSetDbgFlag(flags);
    //AllocConsole();
    //freopen("CONOUT$", "w", stdout);
#endif 

    // check args
    if (argc == 2) {
        TestFile(argv[1]);
        return 0;
    }

    TestRegression();
    TestComments();
    TestCdata();
    TestWhitespace();
    TestAttributes();
    TestNamespaces();
    TestSerializer();
    TestCanonicalizer();

    return 0;
}
