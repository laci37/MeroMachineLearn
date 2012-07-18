/*
 * CMatrix.cpp
 *
 *  Created on: 2012.07.18.
 *      Author: mero
 */

#include "include/CMatrix.h"

CMatrix::CMatrix() {
	this->arr = new double[1][1];
	this->rows = 1;
	this->cols = 1;
	arr[0][0] = 0;
}

CMatrix::CMatrix(int rows, int cols) {
	this->arr = new double[rows][cols];
	this->rows = rows;
	this->cols = cols;
	for (int i = 0; i < rows; i++)
		for (int j = 0; j < rows; j++) {
			arr[i][j] = 0;
		}
}

CMatrix::CMatrix(int rows, int cols, double initval) {
	this->arr = new double[rows][cols];
	this->rows = rows;
	this->cols = cols;
	for (int i = 0; i < rows; i++)
		for (int j = 0; j < cols; j++) {
			arr[i][j] = initval;
		}
}

CMatrix::CMatrix(int rows, int cols, double initarr[][]) {
	this->rows = rows;
	this->cols = cols;
	arr = initarr;
}

const double CMatrix::get(const int row, const int col) {
	return arr[row][col];
}

void CMatrix::set(int row, int col, double value) {
	arr[row][col] = value;
}

CMatrix& CMatrix::operator =(const CMatrix that) {
	for (int i = 0; i < rows; i++) {
		delete[] arr[i];
	}
	this->rows = that.rows;
	this->cols = that.cols;
	this->arr = new double[this->rows][this->cols];
	for (int i = 0; i < rows; i++)
		for (int j = 0; j < cols; j++) {
			arr[i][j] = that.get(i, j);
		}
	return this;
}

CMatrix& CMatrix::operator *=(const CMatrix that) {
	for (int i = 0; i < rows; i++)
		for (int j = 0; j < cols; j++) {
			arr[i][j] = that.get;
		}
}

CMatrix& CMatrix::operator *=(const double that) {
}

CMatrix& CMatrix::operator +=(const CMatrix that) {
}

CMatrix& CMatrix::operator -=(const CMatrix that) {
}

const CMatrix CMatrix::operator *(const CMatrix that) {
}

const CMatrix CMatrix::operator *(const double that) {
}

const CMatrix CMatrix::operator +(const CMatrix that) {
}

const CMatrix CMatrix::operator -(const CMatrix that) {
}

const CVector CMatrix::operator [](const int subscript) {
}

CMatrix::~CMatrix() {
	// TODO Auto-generated destructor stub
	for (int i = 0; i < rows; i++) {
		delete[] arr[i];
	}

}

