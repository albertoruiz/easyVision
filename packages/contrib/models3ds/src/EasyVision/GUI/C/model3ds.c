#include <lib3ds/file.h>
#include <lib3ds/camera.h>
#include <lib3ds/mesh.h>
#include <lib3ds/node.h>
#include <lib3ds/material.h>
#include <lib3ds/matrix.h>
#include <lib3ds/vector.h>
#include <lib3ds/light.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <GL/glew.h>
#include <GL/glut.h>


Lib3dsFile * load3ds(char* name) {
    Lib3dsFile * model;
    model = lib3ds_file_load(name);
    if(!model) {
        printf("Error: wrong model3ds file path: %s\n", name);
        exit(1);
    }
//    printf("nmeshes = %d\n", model -> frames);
    return model;
}

typedef struct bmodel {
    int totFaces;
    GLuint vertexVBO;
    GLuint normalVBO;
} bmodel;


bmodel* store3ds (Lib3dsFile * model) {
    GLuint m_VertexVBO, m_NormalVBO;
    int m_TotalFaces = 0;
    
    {Lib3dsMesh * mesh;
    // Loop through every mesh
    for(mesh = model->meshes;mesh != NULL;mesh = mesh->next)
    {
            // Add the number of faces this mesh has to the total faces
            m_TotalFaces += mesh->faces;
    }
//    printf("faces = %d\n",m_TotalFaces);
    }
        
        // Allocate memory for our vertices and normals
    Lib3dsVector * vertices = (Lib3dsVector *) malloc (sizeof(Lib3dsVector) * m_TotalFaces * 3);
    Lib3dsVector * normals = (Lib3dsVector *) malloc (sizeof(Lib3dsVector) * m_TotalFaces * 3);

    Lib3dsMesh * mesh;
    unsigned int FinishedFaces = 0;
    // Loop through all the meshes
    for(mesh = model->meshes;mesh != NULL;mesh = mesh->next)
    {
            lib3ds_mesh_calculate_normals(mesh, &normals[FinishedFaces*3]);
            // Loop through every face
            for(unsigned int cur_face = 0; cur_face < mesh->faces;cur_face++)
            {
                    Lib3dsFace * face = &mesh->faceL[cur_face];
                    for(unsigned int i = 0;i < 3;i++)
                    {
                            memcpy(&vertices[FinishedFaces*3 + i], mesh->pointL[face->points[ i ]].pos, sizeof(Lib3dsVector));
                    }
                    FinishedFaces++;
            }
    }

//    printf("copied\n");

    glewInit();

    // Generate a Vertex Buffer Object and store it with our vertices
    glGenBuffers(1, &m_VertexVBO);
    glBindBuffer(GL_ARRAY_BUFFER, m_VertexVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(Lib3dsVector) * 3 * m_TotalFaces, vertices, GL_STATIC_DRAW);

    

    // Generate another Vertex Buffer Object and store the normals in it
    glGenBuffers(1, &m_NormalVBO);
    glBindBuffer(GL_ARRAY_BUFFER, m_NormalVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(Lib3dsVector) * 3 * m_TotalFaces, normals, GL_STATIC_DRAW);

//    printf("buffered\n");

    // Clean up our allocated memory
    free(vertices);
    free(normals);

    // We no longer need lib3ds
    lib3ds_file_free(model);
    model = NULL;
//    printf("finished\n");
    
    bmodel * pmodel = (bmodel *) malloc(sizeof(bmodel));
    pmodel -> totFaces = m_TotalFaces;
    pmodel -> vertexVBO = m_VertexVBO;
    pmodel -> normalVBO = m_NormalVBO;
    return pmodel;
}


void renderit(bmodel * m) {
        //assert(m_TotalFaces != 0);
       
        // Enable vertex and normal arrays
        glEnableClientState(GL_VERTEX_ARRAY);
        glEnableClientState(GL_NORMAL_ARRAY);
       
        // Bind the vbo with the normals
        glBindBuffer(GL_ARRAY_BUFFER, m -> normalVBO);
        // The pointer for the normals is NULL which means that OpenGL will use the currently bound vbo
        glNormalPointer(GL_FLOAT, 0, NULL);
       
        glBindBuffer(GL_ARRAY_BUFFER, m->vertexVBO);
        glVertexPointer(3, GL_FLOAT, 0, NULL);
       
        // Render the triangles
        glDrawArrays(GL_TRIANGLES, 0, m->totFaces * 3);
       
        glDisableClientState(GL_VERTEX_ARRAY);
        glDisableClientState(GL_NORMAL_ARRAY);
}

