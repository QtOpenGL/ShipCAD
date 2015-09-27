/*##############################################################################################
 *    ShipCAD                                                                                  *
 *    Copyright 2015, by Greg Green <ggreen@bit-builder.com>                                   *
 *    Original Copyright header below                                                          *
 *                                                                                             *
 *    This code is distributed as part of the FREE!ship project. FREE!ship is an               *
 *    open source surface-modelling program based on subdivision surfaces and intended for     *
 *    designing ships.                                                                         *
 *                                                                                             *
 *    Copyright © 2005, by Martijn van Engeland                                                *
 *    e-mail                  : Info@FREEship.org                                              *
 *    FREE!ship project page  : https://sourceforge.net/projects/freeship                      *
 *    FREE!ship homepage      : www.FREEship.org                                               *
 *                                                                                             *
 *    This program is free software; you can redistribute it and/or modify it under            *
 *    the terms of the GNU General Public License as published by the                          *
 *    Free Software Foundation; either version 2 of the License, or (at your option)           *
 *    any later version.                                                                       *
 *                                                                                             *
 *    This program is distributed in the hope that it will be useful, but WITHOUT ANY          *
 *    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          *
 *    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 *
 *                                                                                             *
 *    You should have received a copy of the GNU General Public License along with             *
 *    this program; if not, write to the Free Software Foundation, Inc.,                       *
 *    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    *
 *                                                                                             *
 *#############################################################################################*/

#include <stdexcept>
#include <iostream>
#include <sstream>
#include <QtGui/QOpenGLShaderProgram>

#include "shader.h"
#include "viewport.h"

using namespace std;
using namespace ShipCAD;

//////////////////////////////////////////////////////////////////////////////////////

Shader::Shader(Viewport* vp)
  : _viewport(vp), _program(0)
{
    // does nothing
}

Shader::~Shader()
{
    delete _program;
}

void Shader::initialize(const char* vertexShaderSource,
            const char* fragmentShaderSource,
            vector<string> uniforms,
            vector<string> attributes)
{
    _program = new QOpenGLShaderProgram(_viewport);
    _program->addShaderFromSourceCode(QOpenGLShader::Vertex, vertexShaderSource);
    _program->addShaderFromSourceCode(QOpenGLShader::Fragment, fragmentShaderSource);
    if (!_program->link())
        cerr << "OpenGL Link failed - Log:\n" << _program->log().toStdString() << endl;
    addUniform("matrix");

    for (size_t i=0; i<uniforms.size(); ++i)
        addUniform(uniforms[i]);
    for (size_t i=0; i<attributes.size(); ++i)
        addAttribute(attributes[i]);
}

void Shader::addUniform(const string& name)
{
    _uniforms[name] = _program->uniformLocation(name.c_str());
    if (_uniforms[name] == -1) {
        ostringstream os;
        os << "bad uniform:" << name;
        cout << os.str() << endl;
//        throw runtime_error(os.str());
    }
}

void Shader::addAttribute(const string& name)
{
    _attributes[name] = _program->attributeLocation(name.c_str());
    if (_attributes[name] == -1) {
        ostringstream os;
        os << "bad attribute:" << name;
        cout << os.str() << endl;
//        throw runtime_error(os.str());
    }
}

void Shader::setWorldMatrix(const QMatrix4x4& matrix)
{
    _program->setUniformValue(_uniforms["matrix"], matrix);
}

//////////////////////////////////////////////////////////////////////////////////////

static const char *LineShaderVertexSource =
    "attribute highp vec4 posAttr;\n"
    "uniform highp mat4 matrix;\n"
    "uniform lowp vec4 sourceColor;\n"
    "varying mediump vec4 color;\n"
    "void main() {\n"
    "   color = sourceColor;\n"
    "   gl_Position = matrix * posAttr;\n"
    "}\n";

static const char *LineShaderFragmentSource =
    "varying mediump vec4 color;\n"
    "void main() {\n"
    "   gl_FragColor = color;\n"
    "}\n";


LineShader::LineShader(Viewport* vp)
  : Shader(vp)
{
    vector<string> attrs;
    vector<string> unis;
    unis.push_back("sourceColor");
    attrs.push_back("posAttr");
    initialize(LineShaderVertexSource, LineShaderFragmentSource, unis, attrs);
}

void LineShader::renderPoints(QVector<QVector3D>& points, QColor color)
{
    _program->setUniformValue(_uniforms["sourceColor"],
                  color.redF(),
                  color.greenF(),
                  color.blueF(),
                  1.0f);

    GLuint posAttr = _attributes["posAttr"];

    _program->setAttributeArray(posAttr, points.constData());
    _program->enableAttributeArray(posAttr);
    glDrawArrays(GL_POINTS, 0, points.size());
    _program->disableAttributeArray(posAttr);
}

void LineShader::renderLines(QVector<QVector3D>& vertices, QColor lineColor)
{
    _program->setUniformValue(_uniforms["sourceColor"],
                  lineColor.redF(),
                  lineColor.greenF(),
                  lineColor.blueF(),
                  1.0f);

    GLuint posAttr = _attributes["posAttr"];

    _program->setAttributeArray(posAttr, vertices.constData());
    _program->enableAttributeArray(posAttr);
    glDrawArrays(GL_LINES, 0, vertices.size());
    _program->disableAttributeArray(posAttr);
}

//////////////////////////////////////////////////////////////////////////////////////

FaceShader::FaceShader(Viewport* vp)
  : Shader(vp)
{
    // does nothing
}

void FaceShader::addUniformsAttributes(vector<string>& uniforms, vector<string>& attributes)
{
    uniforms.push_back("sourceColor");
    attributes.push_back("vertex");
    attributes.push_back("vnormal");
}

//////////////////////////////////////////////////////////////////////////////////////

static const char* MonoShaderVertexSource =
	"attribute vec4 vertex;"
	"attribute vec3 vnormal;"
	"uniform mat4 matrix;"
    "uniform vec4 sourceColor;"
    "varying vec4 color;"
	"void main(void)"
	"{"
    "    vec3 L = normalize(vec3(0, 1, 1));"
    "    float angle = max(dot(vnormal, L), 0.0);"
    "    color = vec4(sourceColor.rgb * 0.2 + sourceColor.rgb * 0.8 * angle, sourceColor.a);"
    "    color = clamp(color, 0.0, 1.0);"
	"    gl_Position = matrix * vertex;"
	"}";

static const char* MonoShaderFragmentSource =
	"varying vec4 color;"
	"void main(void)"
	"{"
	"    gl_FragColor = color;"
	"}";

MonoFaceShader::MonoFaceShader(Viewport* vp)
  : FaceShader(vp)
{
    vector<string> uniforms;
    vector<string> attributes;
    addUniformsAttributes(uniforms, attributes);
    initialize(MonoShaderVertexSource, MonoShaderFragmentSource, uniforms, attributes);
}

void MonoFaceShader::renderMesh(QColor meshColor,
                                QVector<QVector3D>& vertices,
                                QVector<QVector3D>& normals)
{
    if (vertices.size() != normals.size())
        throw runtime_error("vertex and normal array not same size MonoFaceShader::renderMesh");

    _program->setUniformValue(_uniforms["sourceColor"],
                  meshColor.redF(),
                  meshColor.greenF(),
                  meshColor.blueF(),
                  1.0f);

    GLuint normalAttr = _attributes["vnormal"];
    GLuint vertexAttr = _attributes["vertex"];

    _program->setAttributeArray(vertexAttr, vertices.constData());
    _program->setAttributeArray(normalAttr, normals.constData());
    _program->enableAttributeArray(normalAttr);
    _program->enableAttributeArray(vertexAttr);
    glDrawArrays(GL_TRIANGLES, 0, vertices.size());
    _program->disableAttributeArray(normalAttr);
    _program->disableAttributeArray(vertexAttr);
}

//////////////////////////////////////////////////////////////////////////////////////

static const char* LightedShaderVertexSource =
	"attribute vec4 vertex;"
	"attribute vec3 vnormal;"
	"uniform mat4 matrix;"
    "uniform mat4 normal;"
    "uniform mat4 modelView;"
    "uniform vec4 sourceColor;"
    "varying vec3 fragNormal;"
    "varying vec3 fragVert;"
	"void main(void)"
	"{"
	"    gl_Position = matrix * vertex;"
    "    vec4 fn = normalize(normal * vec4(vnormal, 1.0));"
    "    fragNormal = vec3(fn);"
    "    fragVert = vec3(modelView * vertex);"
	"}";

static const char* LightedShaderFragmentSource =
    "// material settings"
    "uniform float materialShininess;"
    "uniform vec3 materialSpecularColor;"
    "uniform vec4 sourceColor;"
    ""
    "uniform struct Light {"
    "    vec3 position;"
    "    vec3 intensities; // aka the color of the light"
    "    float attenuation;"
    "    float ambientCoefficient;"
    "} light;"
    ""
	"varying vec3 fragNormal;"
	"varying vec3 fragVert;"
    ""
    "out vec4 color;"
	"void main(void)"
	"{"
    "    vec3 L = normalize(light.position - fragVert);"
    "    vec3 E = normalize(-fragVert);"
    "    vec3 R = normalize(-reflect(L,N));"
    ""
    "    // calculate ambient term"
    "    vec3 Iamb = light.ambientCoefficient * sourceColor.rgb * light.intensities;"
    ""
    "    // calculate diffuse term"
    "    float diffuseCoefficient = max(dot(fragNormal, L), 0.0);"
    "    vec3 Idiff = diffuseCoefficient * sourceColor.rgb * light.intensities;"
    ""
    "    // calculate specular term"
    "    float specularCoefficient = 0.0;"
    "    if(diffuseCoefficient > 0.0)"
    "        specularCoefficient = pow(max(dot(R,E),0.0), materialShininess);"
    "    vec3 Ispec = specularCoefficient * materialSpecularColor * light.intensities;"
    ""
    "    // attenuation"
    "    float distanceToLight = length(light.position - fragVert);"
    "    float attenuation = 1.0 / (1.0 + light.attenuation * pow(distanceToLight, 2));"
    ""
    "    // linear color"
    "    vec3 linearColor = Iamb + attenuation*(Idiff + Ispec);"
    ""
    "    // final color"
    "    vec3 gamma = vec3(1.0/2.2);"
	"    color = vec4(pow(linearColor, gamma), sourceColor.a);"
	"}";

LightedFaceShader::LightedFaceShader(Viewport* vp)
  : FaceShader(vp)
{
    vector<string> uniforms;
    vector<string> attributes;
    addUniformsAttributes(uniforms, attributes);
    //uniforms.push_back("proj");
    uniforms.push_back("normal");
    uniforms.push_back("modelView");
    uniforms.push_back("materialShininess");
    uniforms.push_back("materialSpecularColor");
    uniforms.push_back("light.position");
    uniforms.push_back("light.intensities");
    uniforms.push_back("light.attenuation");
    uniforms.push_back("light.ambientCoefficient");
    initialize(LightedShaderVertexSource, LightedShaderFragmentSource, uniforms, attributes);
}

void LightedFaceShader::setProjMatrix(const QMatrix4x4& matrix)
{
    _program->setUniformValue(_uniforms["proj"], matrix);
}

void LightedFaceShader::setNormalMatrix(const QMatrix4x4& matrix)
{
    _program->setUniformValue(_uniforms["normal"], matrix);
}

void LightedFaceShader::setModelViewMatrix(const QMatrix4x4& matrix)
{
    _program->setUniformValue(_uniforms["modelView"], matrix);
}

void LightedFaceShader::renderMesh(QColor meshColor,
                                QVector<QVector3D>& vertices,
                                QVector<QVector3D>& normals)
{
    if (vertices.size() != normals.size())
        throw runtime_error("vertex and normal array not same size LightedFaceShader::renderMesh");

    _program->setUniformValue(_uniforms["sourceColor"],
                  meshColor.redF(),
                  meshColor.greenF(),
                  meshColor.blueF(),
                  1.0f);
    _program->setUniformValue(_uniforms["materialShininess"], 80.0f);
    _program->setUniformValue(_uniforms["materialSpecularColor"], 1.0f, 1.0f, 1.0f);
    _program->setUniformValue(_uniforms["light.position"], 0.0f, 0.0f, 1.0f);
    _program->setUniformValue(_uniforms["light.intensities"], 1.0f, 1.0f, 1.0f);
    _program->setUniformValue(_uniforms["light.attenuation"], 0.2f);
    _program->setUniformValue(_uniforms["light.ambientCoefficient"], 0.05f);
#if 0
    _program->setUniformValue(_uniforms["diffuseColor"],1.0f,0.0f,0.0f,1.0f);
#endif
    
    // lighting setup
    //glEnable( GL_LIGHTING );
    //glEnable( GL_LIGHT0 );
    //glEnable( GL_NORMALIZE );
    //glShadeModel( GL_SMOOTH );
    
    //float ltmka[] = {0.0, 0.0, 0.0, 0.0};
    //glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ltmka);
    //glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER, 1.0);
    //glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, 0.0);
    
 	// set lighting position
    //float ltpos[] = {0, 1.0, 1.0};
    //glLightfv(GL_LIGHT0, GL_POSITION, ltpos);

    // set lighting intensity and color
    //float ltambient[] = {0.0, 0.0, 0.0, 1.0};
    //float ltdiffuse[] = {1.0, 1.0, 1.0, 1.0};
    //float ltspecular[] = {1.0, 1.0, 1.0, 1.0};
    //glLightfv(GL_LIGHT0, GL_AMBIENT, ltambient);
    //glLightfv(GL_LIGHT0, GL_DIFFUSE, ltdiffuse);
    //glLightfv(GL_LIGHT0, GL_SPECULAR, ltspecular);

    // material parameters
    //float matspecular[] = {0.8, 0.8, 0.8, 1.0};
    //float matemission[] = {0.2, 0.2, 0.2, 0.0};
    //glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, matspecular);
    //glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, matemission);
    //glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 20.0);

    //glEnable(GL_COLOR_MATERIAL);
    //glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    //glColor4f(meshColor.redF(), meshColor.greenF(), meshColor.blueF(), meshColor.alphaF());
    
    GLuint normalAttr = _attributes["vnormal"];
    GLuint vertexAttr = _attributes["vertex"];

    _program->setAttributeArray(vertexAttr, vertices.constData());
    _program->setAttributeArray(normalAttr, normals.constData());
    _program->enableAttributeArray(normalAttr);
    _program->enableAttributeArray(vertexAttr);
    glDrawArrays(GL_TRIANGLES, 0, vertices.size());
    _program->disableAttributeArray(normalAttr);
    _program->disableAttributeArray(vertexAttr);

    //glDisable( GL_NORMALIZE );
    //glDisable( GL_COLOR_MATERIAL );
    //glDisable( GL_LIGHT0 );
    //glDisable( GL_LIGHTING );
}

//////////////////////////////////////////////////////////////////////////////////////

